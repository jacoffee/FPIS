package ch03;

/**
 * Created by allen on 14-12-9.
 */
public class Variance {
	public static void main(String[] args) {
		String[] a1 = {"abc"};
		Object[] a2 = a1;
		a2[0] = new Integer(10);
		// This will throw  ArrayStoreException
		/*
			What happens here is that Java stores the element type of the array at runtime. Then, every time an array element is updated, the new element value
			is checked against the stored type. If it is not an instance of that type, an ArrayStore exception is thrown
		*/
		String a3 = a1[0];
	}
}
