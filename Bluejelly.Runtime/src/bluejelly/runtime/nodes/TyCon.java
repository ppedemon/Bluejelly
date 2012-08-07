/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.runtime.nodes;

import java.util.Arrays;

import bluejelly.runtime.ExecutionContext;

/**
 * Type constructor node.
 * 
 * @author ppedemon
 */
public class TyCon implements Node {

    // Cache for zero-ary constructors
    private static final TyCon[] zCache = new TyCon[1 << 5];
    
    static {
        for (int i = 0; i < zCache.length; i++) {
            zCache[i] = new TyCon(i);
        }
    }
    
    /**
     * Get a zero-ary type constructor.
     * @param tag    intended tag
     * @return       zero-ary type constructor with tag <code>tag</code>
     */
    public static TyCon mkzTyCon(int tag) {
        if (tag >= zCache.length) {
            return new TyCon(tag);
        } else {
            return zCache[tag];
        }
    }
    
    // Type constructor tag
    private final int tag;
    
    // Nodes in this type constructor
    private Node[] nodes;
    
    /**
     * Default constructor.
     */
    public TyCon(int tag) {
        this(tag, new Node[0]);
    }
    
    /**
     * Construct a type constructor for some tag and nodes.
     * @param tag      type constructor tag
     * @param nodes    nodes packed by the constructor
     */
    public TyCon(int tag, Node[] nodes) {
        this.tag = tag;
        this.nodes = nodes;
    }
    
    /**
     * Get tag for this type constructor.
     * @return    tag for this type constructor
     */
    public int getTag() {
        return this.tag;
    }
    
    /**
     * Get nodes packed by this type constructor.
     * @return     nodes packed by this type constructor
     */
    public Node[] getNodes() {
        return this.nodes;
    }
    
    /**
     * Set the nodes packed by this type constructor.
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
        ctx.whnf();
    }

    @Override
    public String toString() {
        return new StringBuilder()
            .append("TyCon{")
            .append(this.tag)
            .append(',')
            .append(Arrays.toString(this.nodes))
            .append('}')
            .toString();
    }

}
