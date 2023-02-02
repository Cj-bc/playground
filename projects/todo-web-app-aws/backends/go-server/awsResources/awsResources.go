package awsResources

import (
	"context"
	"fmt"
	"github.com/aws/aws-sdk-go-v2/config"
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
