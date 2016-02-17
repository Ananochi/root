import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Props;
import akka.actor.UntypedActor;


import java.io.Serializable;

import java.util.Collections;
import java.util.List;

/**
 * Created by rloc@itu.dk on 12-01-2016.
 */

public class SortingPipelineAKKA {

    public static void main(String args[]) throws InterruptedException {

        final ActorSystem system = ActorSystem.create("SortingPipeSystem");

        // TODO: Actors
        final ActorRef First = system.actorOf(Props.create(Sorter.class), "First");
        final ActorRef Second = system.actorOf(Props.create(Sorter.class), "Second");
        final ActorRef Echo = system.actorOf(Props.create(Echo.class), "Echo");

        First.tell(new InitMessage(Second), ActorRef.noSender());
        Second.tell(new InitMessage(Echo), ActorRef.noSender());

        int input[] = {4,7,2,8,6,1,5,3};
        int washout[] = {9,9,9,9,9,9,9,9};

        for (int x=0; x<input.length; x++){
            First.tell(new NumMessage(input[x]), ActorRef.noSender());
        }

        for (int x=0; x<washout.length; x++){
            First.tell(new NumMessage(washout[x]), ActorRef.noSender());
        }

}

}

// - MESSAGES -------------------------

class InitMessage implements Serializable {

    public final ActorRef pid;

    public InitMessage(ActorRef pid) {this.pid=pid;}

}

class NumMessage implements Serializable {

    public final int n;

    public NumMessage(int n) {this.n=n;}

}

// - ACTORS ---------------------------

class Sorter extends UntypedActor {

    List<Integer> L;
    ActorRef Out;

    @Override
    public void onReceive(Object o) throws Exception {

        // the first stage
        if (o instanceof InitMessage) {
            InitMessage initMsg = (InitMessage) o;
            Out = ((InitMessage) o).pid;
        }

        // later stages
        if (o instanceof NumMessage) {
            NumMessage numMsg = (NumMessage) o;

            while (L.size() < 4) {
                L.add(numMsg.n);
                Collections.sort(L);
                // Out missing??
            }

            L.add(numMsg.n);
            Out.tell(new NumMessage(numMsg.n), getSelf());
        }
    }
}

class Echo extends UntypedActor {

    @Override
    public void onReceive(Object o) throws Exception {
        if (o instanceof NumMessage) {

            NumMessage numMsg = (NumMessage) o;
            System.out.println(numMsg.n);
        }
    }
}

