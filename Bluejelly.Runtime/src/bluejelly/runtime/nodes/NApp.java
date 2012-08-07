package bluejelly.runtime.nodes;

import java.util.Arrays;

import bluejelly.runtime.ExecutionContext;

/**
 * Non-updatable node. This usually corresponds to a partial application,
 * which can't be evaluated (and hence, updated).
 * 
 * @author ppedemon
 *
 */
public class NApp implements Node {

    /*
     * Nodes in the applicacion. This follows the same convention as
     * the runtime stack, that is: top of the stack is at the right.
     * 
     * So, MkApp(n), with stack [...,xn,xn-1,...,x1] gives: 
     *    NApp[xn,xn-1,...,x1]
     *    
     * Array index increases to the right, which means:
     *    this.nodes[0] = xn, this.nodes[n-1] = x1. 
     *    
     *  In general:
     *     this.nodes[i] = x{n-i}, i in [0,n-1]
     */
    private Node[] nodes;
    
    /**
     * Default constructor.
     */
    public NApp() {
        this(new Node[0]);
    }
    
    /**
     * Construct a new instance with the given nodes.
     * @param nodes    nodes in the NApp
     */
    public NApp(Node[] nodes) {
        this.nodes = nodes;
    }

    /**
     * Get child nodes for this napp.
     * @return    child nodes
     */
    public Node[] getNodes() {
        return this.nodes;
    }

    /**
     * Set nodes packed by this application.
     * @param nodes    nodes to pack
     */
    public void pack(Node[] nodes) {
        this.nodes = nodes;
    }
    
    /* (non-Javadoc)
     * @see org.bluejelly.nodes.Node#enter(bluejelly.runtime.ExecutionContext)
     */
    @Override
    public void enter(ExecutionContext ctx) {
        --ctx.sp;
        ctx.stackCheck(this.nodes.length);
        for (Node n: this.nodes) {
            ctx.s[++ctx.sp] = n;
        }
    }

    @Override
    public String toString() {
        return new StringBuffer()
            .append("NApp:")
            .append(Arrays.toString(this.nodes))
            .toString();
    }
}
