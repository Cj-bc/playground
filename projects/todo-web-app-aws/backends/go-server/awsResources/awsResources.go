package awsResources

import (
	"context"
	"fmt"
	"github.com/aws/aws-sdk-go-v2/config"
	"github.com/aws/aws-sdk-go-v2/feature/ec2/imds"
	"github.com/aws/aws-sdk-go-v2/service/ec2"
	"github.com/aws/aws-sdk-go-v2/service/ec2/types"
)

func createClient(ctx context.Context) (*ec2.Client, error) {
	cfg, err := config.LoadDefaultConfig(ctx, config.WithRegion("ap-northeast-1"))
	if err != nil {
		return nil, fmt.Errorf("Failed to make AWS config: %w", err)
	}
	return ec2.NewFromConfig(cfg), nil
}

func SelfIpAddress(ctx context.Context) (string, error) {
	cfg, err := config.LoadDefaultConfig(ctx, config.WithRegion("ap-northeast-1"))
	if err != nil {
		return "", fmt.Errorf("Failed to make AWS config: %w", err)
	}

	imdsClient := imds.NewFromConfig(cfg)
	doc, err := imdsClient.GetInstanceIdentityDocument(ctx, &imds.GetInstanceIdentityDocumentInput{})
	if err != nil {
		return "", fmt.Errorf("failed to retrive document: %w", err)
	}

	ec2Client, err := createClient(ctx)
	if err != nil {
		return "", fmt.Errorf("failed querying self ip address: %w", err)
	}

	desc, err := ec2Client.DescribeInstances(ctx, &ec2.DescribeInstancesInput{InstanceIds: []string{doc.InstanceID}})
	if err != nil {
		return "", fmt.Errorf("failed retriving instance description: %w", err)
	}
	if len(desc.Reservations) == 0 || len(desc.Reservations[0].Instances) == 0 {
		return "", fmt.Errorf("instance description was empty: %w", err)
	}
	return *desc.Reservations[0].Instances[0].PublicIpAddress, nil

}

// 今回は1度しか呼び出さないので、内部で ec2.Client を持たせる作りにしてしまう
// フロントサーバーが複数ある場合はその全てを返す
func FrontIpAddress(ctx context.Context) ([]string, error) {
	c, err := createClient(ctx)
	if err != nil {
		return nil, err
	}

	filterName := "tag:Name"
	input := &ec2.DescribeInstancesInput{
		Filters: []types.Filter{{Name: &filterName, Values: []string{"a20dc036-todo-app-front"}}},
	}
	output, err := c.DescribeInstances(ctx, input)
	if err != nil {
		return nil, err
	}
	var addrs []string
	for _, reservation := range output.Reservations {
		for _, instance := range reservation.Instances {
			addrs = append(addrs, *instance.PrivateIpAddress)
		}
	}

	return addrs, nil
}
