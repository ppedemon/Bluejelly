package bluejelly.runtime;

/**
 * Runtime configuration. Stack sizes must multiply of <code>BSIZE</code>.
 * 
 * @author ppedemon
 */
public final class Config {
	
	/**
	 * Output style.
	 */
	public static enum OutStyle {DEFAULT, LIST, BOOL};
	
	/**
	 * Block size
	 */
	public static int BSIZE = 1 << 10;
	
	/**
	 * Default initial stack size.
	 */
	public static int INIT_STACK_SIZE = BSIZE << 1;
	
	/**
	 * Default initial marker stack size.
	 */
	private static int INIT_MARKER_STACK_SIZE = BSIZE;
	
	/**
	 * Default maximum stack size.
	 */
	private static int MAX_STACK_SIZE = BSIZE << 2;
	
	/**
	 * Default maximum marker stack size.
	 */
	private static int MAX_MARKER_STACK_SIZE = BSIZE << 1;
	
	/**
	 * Initial stack size.
	 */
	public int initStackSize = INIT_STACK_SIZE;
	
	/**
	 * Maximum stack size.
	 */
	public int maxStackSize = MAX_STACK_SIZE;
	
	/**
	 * Initial marker stack size.
	 */
	public int initMarkerStackSize = INIT_MARKER_STACK_SIZE;
	
	/**
	 * Maximum marker stack size.
	 */
	public int maxMarkerStackSize = MAX_MARKER_STACK_SIZE;
	
	/**
	 * Desired output style.
	 */
	public OutStyle style = OutStyle.DEFAULT;
	
}
