// A system module containing protocol access constructs
// Module objects referenced with 'http:' in code
import ballerina/http;

# A service is a network-accessible API
# Advertised on '/hello', port comes from listener endpoint
service hello on new http:Listener(9090) {

    # A resource is an invokable API method
    # Accessible at '/hello/sayHello
    # 'caller' is the client invoking this resource 

    # + caller - Server Connector
    # + request - Request
    resource function sayHello(http:Caller caller, http:Request request) {

        // Create object to carry data back to caller
        http:Response response = new;

        // Set a string payload to be sent to the caller
        response.setTextPayload("Hello Ballerina!");

        // Send a response back to caller
        // Errors are ignored with '_'
        // -> indicates a synchronous network-bound call
        _ = caller -> respond(response);
    }
}
