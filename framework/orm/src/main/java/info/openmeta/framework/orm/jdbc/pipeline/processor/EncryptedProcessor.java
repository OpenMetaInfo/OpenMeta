package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.encrypt.EncryptUtils;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.utils.ListUtils;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Map;

/**
 * Encrypted field processor
 */
public class EncryptedProcessor extends StringProcessor {

    public EncryptedProcessor(MetaField metaField) {
        super(metaField);
    }

    /**
     * Batch encryption of encrypted fields for input rows.
     *
     * @param rows      List of rows to be processed
     * @param accessType Access type, such as READ, CREATE, UPDATE
     */
    @Override
    public void batchProcessInputRows(List<Map<String, Object>> rows, AccessType accessType) {
        // Extract the plaintext dictionary:
        // index-plaintext structure, used for batch encryption, ignoring null values and empty strings
        Map<Integer, String> plaintextMap = ListUtils.extractValueIndexMap(rows, fieldName);
        if (!CollectionUtils.isEmpty(plaintextMap)) {
            // Batch encryption and replacement of plaintext
            Map<Integer, String> ciphertextMap = EncryptUtils.encrypt(plaintextMap);
            ciphertextMap.forEach((index, encryptedValue) -> {
                rows.get(index).put(fieldName, encryptedValue);
            });
        }
    }

    /**
     * Batch decryption of encrypted fields for output rows.
     *
     * @param rows List of rows to be processed
     */
    @Override
    public void batchProcessOutputRows(List<Map<String, Object>> rows) {
        // Extract a map of ciphertext: index-ciphertext for batch decryption, ignoring null and empty strings.
        Map<Integer, String> ciphertextMap = ListUtils.extractValueIndexMap(rows, fieldName);
        if (!CollectionUtils.isEmpty(ciphertextMap)) {
            // Batch decryption and replacement of ciphertext
            Map<Integer, String> plaintextMap = EncryptUtils.decrypt(ciphertextMap);
            plaintextMap.forEach((index, plaintext) -> {
                rows.get(index).put(fieldName, plaintext);
            });
        }
    }

}
