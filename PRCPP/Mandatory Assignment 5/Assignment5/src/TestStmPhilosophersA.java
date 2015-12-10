// For week 10
// sestoft@itu.dk * 2014-11-05, 2015-10-14

// Compile and run like this:
//   javac -cp ~/lib/multiverse-core-0.7.0.jar TestStmPhilosophersA.java 
//   java -cp ~/lib/multiverse-core-0.7.0.jar:. TestStmPhilosophersA

// The Dining Philosophers problem, due to E.W. Dijkstra 1965.  Five
// philosophers (threads) sit at a round table on which there are five
// forks (shared resources), placed between the philosophers.  A
// philosopher alternatingly thinks and eats spaghetti.  To eat, the
// philosopher needs exclusive use of the two forks placed to his left
// and right, so he tries to lock them.  

// Both the places and the forks are numbered 0 to 5.  The fork to the
// left of place p has number p, and the fork to the right has number
// (p+1)%5.

// For the Multiverse library:
import org.multiverse.api.references.*;
import static org.multiverse.api.StmUtils.*;

public class TestStmPhilosophersA {
  public static void main(String[] args) {
    final TxnBoolean[] forks = 
      { newTxnBoolean(), newTxnBoolean(), newTxnBoolean(), newTxnBoolean(), newTxnBoolean() };
    for (int place=0; place<forks.length; place++) {
      Thread phil = new Thread(new PhilosopherA(forks, place));
      phil.start();
    }
  }
}

class PhilosopherA implements Runnable {
  private final TxnBoolean[] forks;
  private final int place;

  public PhilosopherA(TxnBoolean[] forks, int place) {
    this.forks = forks;
    this.place = place;
  }
  
  public void run() {
    while (true) {
      // Take the two forks to the left and the right
      final int left = place, right = (place+1) % forks.length;
      atomic(() -> { 
	// System.out.printf("[%d]", place); 
        if (!forks[left].get() && !forks[right].get()) {
          forks[left].set(true); 
          forks[right].set(true); 
        } else
          retry();
      });
      System.out.printf("%d ", place);  // Eat
      atomic(() -> { 
        forks[left].set(false); 
        forks[right].set(false);
      });
      try { Thread.sleep(10); }         // Think
      catch (InterruptedException exn) { }
    }
  }
}
