package bluejelly.runtime.nodes;

import bluejelly.runtime.ExecutionContext;

/**
 * A heap node. Heap nodes can be <em>entered</em>, 
 * that is, evaluated to normal form.
 * 
 * @author ppedemon
 */
public interface Node {
	
	/**
	 * Enter this node, evaluating it to normal form.
	 * @param ctx    {@link ExecutionContext} where this node is executing
	 */
	void enter(ExecutionContext ctx);
	
}
