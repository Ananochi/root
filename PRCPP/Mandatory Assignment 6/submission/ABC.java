import java.util.Random; import java.io.*; import akka.actor.*;
import akka.actor.dsl.Creators;

// -- BASIC PROCESSING ------------------------------------------
class basicProcessing {

    public static int randomNum(int input) {

        Random rand = new Random();
        int randomNum = rand.nextInt((input - 1) + 1) + 1;

        return randomNum;
    }
}

// -- MESSAGES --------------------------------------------------

class StartTransferMessage implements Serializable {

    public final ActorRef bank;
    public final ActorRef from;
    public final ActorRef to;

    public StartTransferMessage(ActorRef b1, ActorRef a1, ActorRef a2) {
        this.bank = b1;
        this.from = a1;
        this.to = a2;
    }

}

class TransferMessage implements Serializable {

    public final int amount;
    public final ActorRef from;
    public final ActorRef to;

    public TransferMessage(int amount, ActorRef from, ActorRef to) {
        this.amount = amount;
        this.from = from;
        this.to = to;
    }

}

class DepositMessage implements Serializable {

    public final int balance;

    public DepositMessage(Integer balance) { this.balance = balance; }
}

class PrintBalanceMessage implements Serializable {
    // TODO ??
}

// -- ACTORS --------------------------------------------------
class AccountActor extends UntypedActor {
    private int balance;

    @Override
    public void onReceive(Object o) throws Exception {
        if (o instanceof DepositMessage) {

            DepositMessage depositMsg = (DepositMessage) o;
            balance = balance + depositMsg.balance;

        }

        if (o instanceof PrintBalanceMessage) {

            System.out.println("Balance = " + balance);

        }
    }
}

class BankActor extends UntypedActor {

    @Override
    public void onReceive(Object o) throws Exception {
        if (o instanceof TransferMessage) {

            TransferMessage transMsg = (TransferMessage) o;
            transMsg.from.tell(new DepositMessage(-transMsg.amount), getSelf());
            transMsg.to.tell(new DepositMessage(transMsg.amount), getSelf());

        }
    }
}

class ClerkActor extends UntypedActor {

    @Override
    public void onReceive(Object o) throws Exception {
        if (o instanceof StartTransferMessage) {

            StartTransferMessage startMsg = (StartTransferMessage) o;
            int random = basicProcessing.randomNum(100);

            ActorRef bank = startMsg.bank;
            ActorRef from = startMsg.from;
            ActorRef to = startMsg.to;

            do {
                bank.tell(new TransferMessage(random, from, to), getSelf());
                random--;
            } while (random>0);
        }
    }
}

// -- MAIN --------------------------------------------------
public class ABC { // Demo showing how things work:
    public static void main(String[] args) throws InterruptedException {
        final ActorSystem system = ActorSystem.create("ABCSystem");

        // Actors
        final ActorRef A1 = system.actorOf(Props.create(AccountActor.class), "A1");
        final ActorRef A2 = system.actorOf(Props.create(AccountActor.class), "A2");
        final ActorRef B1 = system.actorOf(Props.create(BankActor.class), "B1");
        final ActorRef B2 = system.actorOf(Props.create(BankActor.class), "B2");
        final ActorRef C1 = system.actorOf(Props.create(ClerkActor.class), "C1");
        final ActorRef C2 = system.actorOf(Props.create(ClerkActor.class), "C2");


        C1.tell(new StartTransferMessage(B1, A1, A2), ActorRef.noSender());
        C2.tell(new StartTransferMessage(B2, A2, A1), ActorRef.noSender());

        try {
            System.out.println("Press return to inspect...");
            System.in.read();

            A1.tell(new PrintBalanceMessage(), ActorRef.noSender());
            A2.tell(new PrintBalanceMessage(), ActorRef.noSender());

            System.out.println("Press return to terminate...");
            System.in.read();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            system.shutdown();
        }
    }
}
