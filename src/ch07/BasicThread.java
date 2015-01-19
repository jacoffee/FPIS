package ch07;

/**
 * Created by allen on 1/17/15.
 */
public class BasicThread extends Thread {
    private int countdown = 5;
    private static int threadCount = 0;

    public BasicThread() {
        super(" " + ++threadCount);
        start();  // 初始化的同时启动该线程 不过考虑Java的单继承， 这总继承Thread的实现并不太好， 这限制了这个类的扩展性
    }

    public String toString() {
        return "#" + getName() + ": " + countdown;
    }

    public void run() {
        while(true) {
            try {
                sleep(100); // 由于SimpleThread是在主分支上启动的，跟主分支应该是平行的 所以它们也是非后台线程
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            System.out.println(this.isDaemon());
            System.out.println(this);
            if (--countdown <=0) return;
        }
    }

    public static void main(String[] args) {
        for (int i=0; i< 5; i++) {
            new BasicThread();
        }

        System.out.println(" 主线程已经到这个地方 ");
    }
}
