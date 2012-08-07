package bluejelly.runtime.nodes;

import bluejelly.runtime.ExecutionContext;

/**
 * A character node.
 * @author ppedemon
 */
public class Char implements Node {

    private static final Char[] charCache = new Char[256];
    
    static {
        for (char c = 0; c < charCache.length; c++)
            charCache[c] = new Char(c);
    }
    
    /**
     * Get a char node for the given character.
     * @param c    value for the node
     * @return     char node with the given value
     */
    public static Char mkChr(char c) {
        if (c < 0 || c >=  charCache.length) {
            return new Char(c);
        } else {
            return charCache[c];
        }
    }
    
    // Node's value
    public final char c;
    
    /**
     * Create a node for the given character.
     * @param c    node's value
     */
    private Char(char c) {
        this.c = c;
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
        return String.valueOf(c);
    }

}
