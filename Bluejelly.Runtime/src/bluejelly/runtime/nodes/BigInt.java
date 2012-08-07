package bluejelly.runtime.nodes;

import java.math.BigInteger;

import bluejelly.runtime.ExecutionContext;

/**
 * Infinite precision integer.
 * @author ppedemon
 */
public class BigInt implements Node {

    public BigInteger i;
    
    /**
     * Construct a new instance wrapping the given {@link BigInteger}
     * @param i    {@link BigInteger} instance to wrap
     */
    public BigInt(BigInteger i) {
        this.i = i;
    }
    
    /* (non-Javadoc)
     * @see bluejelly.runtime.nodes.Node#enter(bluejelly.runtime.ExecutionContext)
     */
    @Override
    public void enter(ExecutionContext ctx) {
        ctx.whnf();
    }

    @Override
    public String toString() {
        return i.toString();
    }
}
