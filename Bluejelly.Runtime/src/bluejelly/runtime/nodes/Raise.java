package bluejelly.runtime.nodes;

import bluejelly.runtime.ExecutionContext;

/**
 * Node signaling an error condition. Whenever entered, this node
 * will raise an exception.
 * 
 * @author ppedemon
 */
public class Raise implements Node {

    // Node for the exception to throw when entering this node
    private final Node ex;
    
    /**
     * Construct a new instance with the given exception node.
     * 
     * @param ex    Exception to throw when entering this node
     */
    public Raise(Node ex) {
        this.ex = ex;
    }
    
    /* (non-Javadoc)
     * @see org.bluejelly.nodes.Node#enter(bluejelly.runtime.ExecutionContext)
     */
    @Override
    public void enter(ExecutionContext ctx) {
        ctx.s[ctx.sp] = this.ex;
        ctx.raise();
    }

    /*
     * (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "Exception: " + this.ex;
    }

}
