package bluejelly.runtime.nodes;

import bluejelly.runtime.ExecutionContext;

/**
 * A boxed int value.
 * 
 * @author ppedemon
 */
public class Int implements Node {

    // Int node cache
    private static Int[] intCache = new Int[1 << 8];
    
    static {
        for (int i = 0; i < intCache.length; i++) 
            intCache[i] = new Int(i);
    }

    /**
     * Construct an int node.
     * 
     * @param i    value for the node
     * @return     int node with the given value
     */
    public static Int mkInt(int i) {
        if (i < 0 || i >= intCache.length) {
            return new Int(i);
        } else {
            return intCache[i];
        }
    }

    // Value for the node
    public final int i;
    
    /**
     * Construct a new instance with the given value.
     * @param i    value for this int vbox.
     */
    private Int(int i) {
        this.i = i;
    }
    
    /* (non-Javadoc)
     * @see org.bluejelly.nodes.Node#enter(bluejelly.runtime.ExecutionContext)
     */
    @Override
    public void enter(ExecutionContext ctx) {
        ctx.whnf();
    }

    @Override
    public String toString() {
        return String.valueOf(this.i);
    }
}
