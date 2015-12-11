import java.util.Random; import java.io.*; import akka.actor.*;

// -- BASIC PROCESSING ------------------------------------------
class basicProcessing {

    public static String n2s(int input) {

        return Integer.toString(input);
    }

    public static int randomNum(int input) {

        Random rand = new Random();
        int randomNum = rand.nextInt((input - 1) + 1) + 1;

        return randomNum;
    }
}

// -- MESSAGES --------------------------------------------------
class StartTransferMessage implements Serializable { /* TODO */

}

class TransferMessage implements Serializable { /* TODO */

}

class DepositMessage implements Serializable {

    public final int amount = 0;
    public final int balance = 0;

    public DepositMessage(Integer balance) { balance = this.amount + this.balance; }
}

class PrintBalanceMessage implements Serializable {

    public final String balance;

    public PrintBalanceMessage(String balance) { this.balance = balance; }
}

// -- ACTORS --------------------------------------------------
class AccountActor extends UntypedActor {
    private int count;

    @Override
    public void onReceive(Object o) throws Exception {

        if (o instanceof MyMessage) {
            MyMessage message = (MyMessage) o;
            System.out.println(message.s + " (" + count + ")");
            count++;
        }
    } /* TODO */
}

class BankActor extends UntypedActor {
    @Override
    public void onReceive(Object o) throws Exception {

    } /* TODO */
}

class ClerkActor extends UntypedActor {
    @Override
    public void onReceive(Object o) throws Exception {

    } /* TODO */
}

// -- MAIN --------------------------------------------------
public class ABC { // Demo showing how things work:
    public static void main(String[] args) {
        final ActorSystem system = ActorSystem.create("ABCSystem");

            /* TODO (CREATE ACTORS AND SEND START MESSAGES) */

        try {
            System.out.println("Press return to inspect...");
            System.in.read();

            /* TODO (INSPECT FINAL BALANCES) */

            System.out.println("Press return to terminate...");
            System.in.read();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            system.shutdown();
        }
    }
}
