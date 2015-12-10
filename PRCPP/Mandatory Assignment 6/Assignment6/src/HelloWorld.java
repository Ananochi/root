// COMPILE:
// javac -cp scala.jar:akka-actor.jar HelloWorld.java 
// RUN:
// java -cp scala.jar:akka-actor.jar:akka-config.jar:. HelloWorld

import java.io.*;
import akka.actor.*;

// -- MESSAGE --------------------------------------------------

class MyMessage implements Serializable { // Object to be communicated (must be Serializable)
    public final String s;
    public MyMessage(String s) { this.s = s; }
}

// -- ACTOR --------------------------------------------------

class MyActor extends UntypedActor { // Our ACTOR...
    private int count = 0; // can have (local) state

    public void onReceive(Object o) throws Exception { // ...that reacts to incoming message:
	if (o instanceof MyMessage) {
	    MyMessage message = (MyMessage) o;
	    System.out.println(message.s + " (" + count + ")");
	    count++;
	}
    }
}

// -- MAIN --------------------------------------------------

public class HelloWorld { // Demo showing how things work:
    public static void main(String[] args) {
	final ActorSystem system = ActorSystem.create("HelloWorldSystem");
	final ActorRef myactor = system.actorOf(Props.create(MyActor.class), "myactor");
	myactor.tell(new MyMessage("hello"), ActorRef.noSender()); // send the message (fire-forget)!
	myactor.tell(new MyMessage("world"), ActorRef.noSender()); // send the message (fire-forget)!
	try {
	    System.out.println("Press return to terminate...");
	    System.in.read();
	} catch(IOException e) {
	    e.printStackTrace();
	} finally {
	    system.shutdown();
	}
    }
}
