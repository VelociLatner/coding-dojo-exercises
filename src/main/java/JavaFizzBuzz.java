import org.junit.Test;
import static org.junit.Assert.assertEquals;

// Fizz Buzz implemented without "if" statements.
public class JavaFizzBuzz {

    private interface Speakable {
        public String speak();
        public int number();
        void markHavingSpoken();
    }

    public class Counter implements Speakable {
        private int value;
        private boolean hasSpoken = false;

        public Counter(int value) {
            this.value = value;
        }

        public String speak() {
            return !hasSpoken ? String.valueOf(value) : "";
        }

        public int number() { return value ;}
        public void markHavingSpoken() { hasSpoken = true; }

    }

    abstract class SpeakableWrapper implements Speakable {
        private Speakable wrappedInstance;

        public SpeakableWrapper(Speakable wrappedInstance) {
            this.wrappedInstance = wrappedInstance;
        }

        public int number() { return wrappedInstance.number(); }

        public String speak() {
            return ((isRelevant() ? makeSound() + " " : "") + wrappedInstance.speak()).trim();
        }

        private String makeSound() {
            markHavingSpoken();
            return sound();
        }

        public void markHavingSpoken() { wrappedInstance.markHavingSpoken(); }

        abstract protected boolean isRelevant();
        abstract protected String sound();
    }

    public class Fizzable extends SpeakableWrapper {

        public Fizzable(Speakable wrappedInstance) { super(wrappedInstance); }

        protected boolean isRelevant() { return number() % 3 == 0; }
        protected String sound() { return "Fizz"; }

    }

    public class Buzzable extends SpeakableWrapper {

        public Buzzable(Speakable wrappedInstance) { super(wrappedInstance); }

        protected boolean isRelevant() { return number() % 5 == 0; }
        protected String sound() { return "Buzz"; }
    }

    public class Bazzable extends SpeakableWrapper {

        public Bazzable(Speakable wrappedInstance) { super(wrappedInstance); }

        protected boolean isRelevant() { return String.valueOf(number()).endsWith("3"); }
        protected String sound() { return "Bazz"; }
    }




    @Test
    public void testBaseCase() {
        assertEquals("1", new Counter(1).speak());
    }

    @Test
    public void test2() {
        assertEquals("2", new Counter(2).speak());
    }

    @Test
    public void testFizz_3() {
        assertEquals("Fizz", new Fizzable(new Counter(3)).speak());
    }

    @Test
    public void testFizz_1() {
        assertEquals("1", new Fizzable(new Counter(1)).speak());
    }

    @Test
    public void testBuzz_5() {
        assertEquals("Buzz", new Buzzable(new Counter(5)).speak());
    }

    @Test
    public void testBuzz_1() {
        assertEquals("1", new Buzzable(new Counter(1)).speak());
    }

    @Test
    public void testFizzBuzz_noBuzz() {
        assertEquals("Fizz", new Fizzable(new Buzzable(new Counter(3))).speak());
    }

    @Test
    public void testFizzBuzz_noFizz() {
        assertEquals("Buzz", new Fizzable(new Buzzable(new Counter(5))).speak());
    }

    @Test
    public void testFizzBuzz_noFizz_noBuzz() {
        assertEquals("1", new Fizzable(new Buzzable(new Counter(1))).speak());
    }

    @Test
    public void testFizzBuzz_Fizz_and_Buzz() {
        assertEquals("Fizz Buzz", new Fizzable(new Buzzable(new Counter(15))).speak());
    }

    @Test
    public void testBazz_13() {
        assertEquals("Bazz", new Bazzable(new Counter(13)).speak());
    }

    @Test
    public void testBazz_12() {
        assertEquals("12", new Bazzable(new Counter(12)).speak());
    }

    @Test
    public void testBazz_withFizz() {
        assertEquals("Fizz Bazz", new Fizzable(new Bazzable(new Counter(33))).speak());
    }


    @Test
    public void FIZZ_BUZZ() {
        for( int x = 0; x < 100; x++) {
            System.out.println(new Fizzable(new Buzzable(new Bazzable(new Counter(x)))).speak());
        }
    }

}
