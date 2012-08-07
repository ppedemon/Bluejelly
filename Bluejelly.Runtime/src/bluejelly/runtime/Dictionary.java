/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */

package bluejelly.runtime;

/**
 * Dictionary containing pointers --this is, names :(--, for 
 * ad-hoc polymorphic functions. Dictionaries might also have
 * pointers to other dictionaries.
 * 
 * @author ppedemon
 */
public class Dictionary {
    
    private final String[] supers;
    private final String[] methods;
    private final String[] specifics;
    
    /**
     * Construct a new dictionary, reflecting an instance of the form:
     * 
     * <code>
     *   class S a => C a where ...
     *   instance (C1 a, ..., Cn a) => C a where ...
     * </code>
     * 
     * @param supers       name of dictionaries for C superclasses (S in the example)
     * @param methods      names of the module methods implementing the class
     * @param specifics    name of C specific dictionaries (C1, ..., Cn)
     */
    public Dictionary(String[] supers, String[] methods, String[] specifics) {
        this.supers = supers;
        this.methods = methods;
        this.specifics = specifics;        
    }
    
    /**
     * Get name of dictionary for superclass number <code>i</code>.
     * @param i    index of intended superclass dictionary
     * @return     name of intended superclass dictionary
     */
    public String getSuper(int i) {
        assert (supers == null || i < supers.length): 
            ("Invalid superclass index: " + i);
        return supers[i];
    }

    /**
     * Get name of method number <code>i</code>.
     * @param i    index of intended method
     * @return     name of intended method
     */
    public String getMethod(int i) {
        assert (methods == null || i < methods.length): 
            ("Invalid method index: " + i);
        return methods[i];
    }

    /**
     * Get name of specific dictionary number <code>i</code>.
     * @param i    index of intended specific dictionary
     * @return     name of intended specific dictionary
     */
    public String getSpecific(int i) {
        assert (specifics == null || i < specifics.length): 
            ("Invalid specifics index: " + i);
        return specifics[i];
    }

}
