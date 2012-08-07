package bluejelly.runtime.nodes;

import bluejelly.runtime.Dictionary;
import bluejelly.runtime.ExecutionContext;
import bluejelly.runtime.JellyRuntimeException;
import bluejelly.runtime.Module;

/**
 * Node wrapping a {@link Dictionary}. We need this because dictionaries
 * are function parameters, i.e., they will be pushed on the node stack.
 * 
 * @author ppedemon
 */
public class Dict implements Node {

	private final Module module;
	private final String dictName;
	private final Dictionary dict;
	
	/**
	 * Construct a new node wrapping the given {@link Dictionary}.
	 * 
	 * @param module      {@link Module} providing the dictionary
	 * @param dictName    dictionary name
	 * @param dict        {@link Dictionary} wrapped by this node
	 */
	public Dict(Module module, String dictName, Dictionary dict) {
		this.module = module;
		this.dictName = dictName;
		this.dict = dict;
	}
	
	/* (non-Javadoc)
	 * @see org.bluejelly.nodes.Node#enter(bluejelly.runtime.ExecutionContext)
	 */
	public Dictionary getDict() {
		return dict;
	}

	@Override
	public String toString() {
		return new StringBuffer()
		.append('<')
		.append(this.module.getClass().getName())
		.append(':')
		.append(this.dictName)
		.append('>')
		.toString();
	}

	@Override
	public void enter(ExecutionContext ctx) {
		// Entering dictionaries is wrong, wrong, wrong
		throw new JellyRuntimeException("Entering dictionary:" + this);
	}

}
