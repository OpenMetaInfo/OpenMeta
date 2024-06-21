package info.openmeta.framework.web.service;

import java.util.Set;

/**
 * Toolkit Service
 */
public interface ToolkitService {

    /**
     * Recompute the stored calculation fields, including computed and cascaded fields.
     *
     * @param modelName model name
     * @param fields fields to be recomputed
     */
    void recompute(String modelName, Set<String> fields);

    /**
     * Encrypts historical plaintext data after the field is set to `encrypted=true`.
     *
     * @param modelName model name
     * @param fieldName field to encrypt historical plaintext data.
     * @return the number of rows fixed
     */
    Long fixUnencryptedData(String modelName, String fieldName);

}
