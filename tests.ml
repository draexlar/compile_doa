let ts = [
	{ name = "Init"; transitions = 
		[ { op = "Status open()"; result = Option [{ label = "OK"; state = NextState "Open" }; { label = "ERROR"; state = NextState "end" }] } ] 
	};
	{ name = "Open"; transitions = 
		[ { op = "Boolean hasNext()"; result = Option [{ label = "TRUE"; state = NextState "Read" }; { label = "FALSE"; state = NextState "Close" }] }; 
			{ op = "void close()"; result = NextState "end" } ] 
	};
	{ name = "Read"; transitions = 
		[ { op = "void read()"; result = NextState "Open" } ] 
	};
	{ name = "Close"; transitions = 
		[ { op = "void close()"; result = NextState "end" } ] 
	};
];;




(* Mungo/examples/collection/CollectionProtocol.protocol *)
let ts = [
	{ name = "Init"; transitions = 
		[ { op = "void initialise(int)"; result = NextState "Empty" } ] 
	};
	{ name = "Empty"; transitions = 
		[ { op = "void put(Node)"; result = NextState "NonEmpty" }; 
			{ op = "void put(Node)"; result = NextState "end" } ] 
	};
	{ name = "NonEmpty"; transitions = 
		[ { op = "void put(Node)"; result = NextState "NonEmpty" }; 
			{ op = "Node get()"; result = NextState "Unknown" } ] 
	};
	{ name = "Unknown"; transitions = 
		[ { op = "void put(Node)"; result = NextState "NonEmpty" }; 
			{ op = "BooleanChoice isEmpty()"; result = Option [{ label = "TRUE"; state = NextState "Empty" }; { label = "FLASE"; state = NextState "NonEmpty" }] } ] 
	};
];;





(* Mungo/examples/collection/StackUser.protocol *)
let ts = [
	{ name = "Init"; transitions = 
		[ { op = "Stack produce(Stack, int)"; result = NextState "Consume" };
			{ op = "Stack produce(Stack)"; result = NextState "Consume" };
			{ op = "void close()"; result = NextState "end" } ]
	};
	{ name = "Consume"; transitions = 
		[ { op = "Stack produce(Stack, int)"; result = NextState "Consume" };
			{ op = "Stack produce(Stack)"; result = NextState "Consume" };
			{ op = "Stack consume(Stack)"; result = NextState "Init" } ]
	};
];;





(* Mungo/examples/TwoParties *)
(* AliceProtocol *)
let ts = [
	{ name = "Init"; transitions = 
		[ { op = "void connect()"; result = InnerState [{ op = "String recvStringFromBob()"; result = NextState "ReceiveChoice" }] } ] 
	};
	{ name = "ReceiveChoice"; transitions = 
		[ { op = "BobChoice choiceFromBob()"; result = Option [{ label = "TIME"; state = NextState "SendTime"}; { label = "GREET"; state = NextState "HowAreYou" }] } ] 
	};
	{ name = "SendTime"; transitions = 
		[ { op = "void sendTimeToBob(int)"; result = NextState "EndProtocol" } ] 
	};
	{ name = "HowAreYou"; transitions = 
		[ { op = "void sendGreetToBob(String)"; result = NextState "EndProtocol" } ] 
	};
	{ name = "EndProtocol"; transitions = 
		[ { op = "void endCommunication()"; result = NextState "end" } ] 
	};
];;

(* BobProtocol *)
let ts = [
	{ name = "Init"; transitions = 
		[ { op = "void connect()"; result = InnerState [{ op = "void sendStringToAlice(String)"; result = NextState "SendChoice" }] } ] 
	};
	{ name = "SendChoice"; transitions = 
		[ { op = "void sendTimeChoiceToAlice()"; result = InnerState [{ op = "int recvTimeFromAlice()"; result = NextState "EndProtocol" }] };
		  { op = "void sendGreetingChoiceToAlice()"; result = InnerState [{ op = "String recvGreetingFromAlice()"; result = NextState "EndProtocol" }] } ] 
	};
	{ name = "EndProtocol"; transitions = 
		[ { op = "void endCommunication()"; result = NextState "end" } ] 
	};
];;





(* Mungo/examples/ThreeParties *)
(* BobProtocol *)
let ts = [
	{ name = "Connect"; transitions = 
		[ { op = "void connect()"; result = NextState "GreetAlice" } ] 
	};
	{ name = "GreetAlice"; transitions = 
		[ { op = "void sendHelloToAlice(String)"; result = NextState "GreetCarol" } ] 
	};
	{ name = "GreetCarol"; transitions = 
		[ { op = "void sendHelloToCarol(String)"; result = NextState "MakeChoice" } ] 
	};
	{ name = "MakeChoice"; transitions = 
		[ { op = "void sendTimeChoiceToAlice()"; result = InnerState [{ op = "int recvTimeFromAlice()"; result = NextState "EndProtocol" }] };
		  { op = "void sendTimeChoiceToCarol()"; result = InnerState [{ op = "int recvTimeFromCarol()"; result = NextState "EndProtocol" }] } ] 
	};
	{ name = "EndProtocol"; transitions = 
		[ { op = "void endCommunication()"; result = NextState "{}" } ] 
	};
];;

(* FriendProtocol *)
let ts = [
	{ name = "Init"; transitions = 
		[ { op = "void connect()"; result = InnerState [{ op = "String recvHelloFromBob()"; result = NextState "ReceiveChoice" }] } ] 
	};
	{ name = "ReceiveChoice"; transitions = 
		[ { op = "BobChoice recvChoiceFromBob()"; result = Option [{ label = "TIME"; state = NextState "SendTime"}; { label = "END"; state = NextState "EndProtocol" }] } ] 
	};
	{ name = "SendTime"; transitions = 
		[ { op = "void sendTimeToBob(int)"; result = NextState "EndProtocol" } ] 
	};
	{ name = "EndProtocol"; transitions = 
		[ { op = "void endCommunication()"; result = NextState "end" } ] 
	};
];;






(* Dimitris-Kouzapas/javatypestate/blob/master/demos/redis/channeloriented/RedisClientProtocol.protocol *)
let ts = [
	{ name = "Init"; transitions = 
		[ { op = "void sendWATCH(String[])"; result = NextState "Watching" } ] 
	};
	{ name = "Watching"; transitions = 
		[ { op = "WatchingLabel sendLabel_Watching(int)"; result = 
				Option [{ label = "GET"; state = InnerState [{ op = "void sendArg_GET(String)"; result =
									InnerState [{ op = "String receiveGET_response()"; result = NextState "Watching" }] }] }; 
								{ label = "WATCH"; state = InnerState [{ op = "void sendArg_WATCH(String[])"; result = NextState "Watching" }] };
								{ label = "MULTI"; state = NextState "Queued" }] 
			} ] 
	};
	{ name = "Queued"; transitions = 
		[ { op = "QueuedLabel sendLabel_Queued(int)"; result = 
				Option [{ label = "SET"; state = InnerState [{ op = "void sendArg0_SET(String)"; result = 
						InnerState [{ op = "void sendArg1_SET(String)"; result = NextState "Queued" }] }] }; 
								{ label = "DISCARD"; state = NextState "end" };
								{ label = "EXEC"; state = InnerState [{ op = "ResultLabel receiveEXEC_response()"; result = 
									Option [{ label = "OK"; state = NextState"end"}; { label = "FAIL"; state = NextState "Queued" }] }] }] 
			} ]
	}
];;







(* ours *)

(* jdmota/behaviour-types-research/blob/infinite/src/BankProtocol.protocol *)
let ts = [
	{ name = "State0"; transitions = 
		[ { op = "void receivePayment()"; result = NextState "State0" } ] 
	};
];;

(* jdmota/behaviour-types-research/blob/working-infinite/src/BankProtocol.protocol *)
let ts = [
	{ name = "State0"; transitions = 
		[ { op = "void receivePayment()"; result = NextState "State0" };
			{ op = "void exit()"; result = NextState "end" } ] 
	};
];;

(* created for testing *)
let ts = [
	{ name = "State0"; transitions = [] }
];;

let ts = [
	{ name = "State0"; transitions = [] };
	{ name = "State1"; transitions = [] }
];;





(* ERRORS *)

(* Should return "State with name 'end' not allowed." *)
let ts = [
	{ name = "end"; transitions = [] };
];;

(* Should return "Found duplicate state" *)
let ts = [
	{ name = "State0"; transitions = [] };
	{ name = "State0"; transitions = [] };
];;

(* Should return "Undifined state: State1" *)
let ts = [
	{ name = "State0"; transitions = 
		[ { op = "void doSomething()"; result = NextState "State1" } ] 
	};
];;

(* Should return "There must be at least an option" *)
let ts = [
	{ name = "State0"; transitions = 
		[ { op = "void doSomething()"; result = Option [] } ] 
	};
];;

(* Should return "Internal choice states must always transition to external choice states" *)
let ts = [
	{ name = "State0"; transitions = 
		[ { op = "void doSomething()"; result = Option [{ label = "L"; state = Option [{ label = "T"; state = NextState "end" }] }] } ] 
	};
];;

(* "Found duplicate method or label: void close()" *)
let ts = [
	{ name = "State0"; transitions = 
		[ { op = "void open()"; result = NextState "end" };
			{ op = "void open(A)"; result = NextState "end" };
			{ op = "void open(A,B)"; result = NextState "end" } ] 
	};
	{ name = "State1"; transitions = 
		[ { op = "void close()"; result = NextState "end" };
			{ op = "void close()"; result = NextState "State0" } ] 
	}
];;

(* "Found duplicate method or label: OK" *)
let ts = [
	{ name = "Init"; transitions = 
		[ { op = "Status open()"; result = Option [{ label = "OK"; state = NextState "end" }; { label = "OK"; state = NextState "end" }; { label = "ERROR"; state = NextState "end" }] } ] 
	};
];;