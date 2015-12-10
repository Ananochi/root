// COMPILE:
// javac -cp scala.jar:akka-actor.jar Ecco.java 
// RUN:
// java -cp scala.jar:akka-actor.jar:akka-config.jar:. Ecco

import java.io.*;
import akka.actor.*;

// -- MESSAGES --------------------------------------------------

class StartMessage implements Serializable {
    public final ActorRef ecco;
    public StartMessage(ActorRef ecco) {
	this.ecco = ecco;
    }
}

class MessageB implements Serializable {
    public final String s;
    public MessageB(String s) {
	this.s = s;
    }
}

// -- ACTORS --------------------------------------------------

class PersonActorB extends UntypedActor {
    public void onReceive(Object o) throws Exception {
	if (o instanceof StartMessage) {
	    StartMessage start = (StartMessage) o;
	    ActorRef ecco = start.ecco;
	    String s = "hvad drikker moller";
	    System.out.println("[says]:  " + s);
	    ecco.tell(new MessageB(s), getSelf());
	} else if (o instanceof MessageB) {
	    MessageB m = (MessageB) o;
	    System.out.println("[hears]: " + m.s);
	}
    }
}

class EccoActor extends UntypedActor {
    public void onReceive(Object o) throws Exception {
	if (o instanceof MessageB) {
	    MessageB m = (MessageB) o;
	    String s = m.s;
	    MessageB reply;
	    if (s.length()>5) reply = new MessageB("..." + s.substring(s.length()-5));
	    else reply = new MessageB("...");
	    getSender().tell(reply, getSelf());
	    getSender().tell(reply, getSelf());
	    getSender().tell(reply, getSelf());
	}
    }
}

// -- MAIN --------------------------------------------------

public class Ecco {
    public static void main(String[] args) {
	final ActorSystem system = ActorSystem.create("EccoSystem");
	final ActorRef person = system.actorOf(Props.create(PersonActorB.class), "person");
	final ActorRef ecco = system.actorOf(Props.create(EccoActor.class), "ecco");
	person.tell(new StartMessage(ecco), ActorRef.noSender());
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
