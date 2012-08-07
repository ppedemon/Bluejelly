package bluejelly.runtime.nodes;

import java.util.Arrays;

import bluejelly.runtime.ExecutionContext;

/**
 * An updatable application.
 * 
 * @author ppedemon
 * @see bluejelly.runtime.nodes.NApp
 */
public class App extends Updatable {
    
    // Nodes of this application, in the same order as in the stack
    // See org.bluejelly.nodes.NApp for the whole story
    private Node[] nodes;
    
    /**
     * Default constructor.
     */
    public App() {
        this(new Node[0]);
    }
    
    /**
     * Construct a new instance with the given nodes.
     * @param nodes    nodes in the NApp
     */
    public App(Node[] nodes) {
        this.nodes = nodes;
    }
    
    /**
     * Get child nodes for this app.
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
     * @see org.bluejelly.nodes.Updatable#reduce(bluejelly.runtime.ExecutionContext)
     */
    @Override
    public void reduce(ExecutionContext ctx) {
        --ctx.sp;
        ctx.stackCheck(this.nodes.length);
        for (Node n: this.nodes) {
            ctx.s[++ctx.sp] = n;
        }
    }
    
    /*
     * (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        if (this.isUpdated()) {
            return this.getInd().toString();
        } else {
            return new StringBuffer()
                .append("App:")
                .append(Arrays.toString(this.nodes))
                .toString();
        }
    }
    
}
