package info.openmeta.framework.orm.encrypt.algorithm;

import info.openmeta.framework.orm.encrypt.Encryptor;
import org.springframework.stereotype.Service;

import javax.crypto.Cipher;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;
import java.security.SecureRandom;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

@Service
public class AESEncryption implements Encryptor {

    private static final String ALGORITHM = "AES";
    private static final String TRANSFORMATION = "AES/CBC/PKCS5Padding";
    private static final int KEY_SIZE = 256;
    private static final int IV_SIZE = 16;
    private static final String CHARSET_NAME = "UTF-8";
    private static final String SECRET_KEY_FACTORY_ALGORITHM = "PBKDF2WithHmacSHA256";
    private static final int ITERATION_COUNT = 10000;

    /**
     * Encrypts the provided data using AES 256 with CBC mode and PKCS5Padding.
     *
     * @param plaintext     The data to encrypt.
     * @param password The password used to generate the encryption key.
     * @return The Base64-encoded encrypted data.
     * @throws Exception if an error occurs during encryption.
     */
    public String encrypt(String plaintext, String password) throws Exception {
        if (plaintext == null || plaintext.isEmpty()) {
            return plaintext;
        }
        byte[] iv = generateRandomIv();
        IvParameterSpec ivParameterSpec = new IvParameterSpec(iv);

        // Generate secret key
        SecretKeySpec secretKey = generateSecretKey(password, iv);

        // Initialize cipher for encryption
        Cipher cipher = Cipher.getInstance(TRANSFORMATION);
        cipher.init(Cipher.ENCRYPT_MODE, secretKey, ivParameterSpec);
        byte[] encryptedBytes = cipher.doFinal(plaintext.getBytes(CHARSET_NAME));

        // Combine IV and encrypted data
        byte[] encryptedIVAndText = combineIvAndEncryptedData(iv, encryptedBytes);

        // Encode combined data to Base64
        return Base64.getEncoder().encodeToString(encryptedIVAndText);
    }

    /**
     * Batch encrypts the provided data.
     * @param plaintextIndexMap Plain text index map
     * @param password The password used to generate the encryption key.
     * @return Index Map of the Base64-encoded encrypted data.
     */
    @Override
    public Map<Integer, String> encrypt(Map<Integer, String> plaintextIndexMap, String password) throws Exception {
        Map<Integer, String> ciphertextIndexMap = new HashMap<>(plaintextIndexMap.size());
        plaintextIndexMap.forEach((index, plaintext) -> {
            try {
                ciphertextIndexMap.put(index, encrypt(plaintext, password));
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
        return ciphertextIndexMap;
    }

    /**
     * Decrypts the provided data which was encrypted using AES 256.
     * Return original string if it cannot be decrypted.
     *
     * @param ciphertext The Base64-encoded encrypted data with IV.
     * @param password        The password used to generate the decryption key.
     * @return The decrypted data as a string.
     * @throws Exception if an error occurs during decryption.
     */
    public String decrypt(String ciphertext, String password) throws Exception {
        if (ciphertext == null || ciphertext.isEmpty()) {
            return ciphertext;
        }
        byte[] encryptedIvTextBytes;
        IvParameterSpec ivParameterSpec;
        try {
            // Decode Base64 data
            encryptedIvTextBytes = Base64.getDecoder().decode(ciphertext);
            // Extract IV
            ivParameterSpec = extractIvFromCombinedData(encryptedIvTextBytes);
        } catch (Exception e) {
            // Return original string if it cannot be decrypted
            return ciphertext;
        }

        // Extract encrypted data
        byte[] encryptedBytes = extractEncryptedDataFromCombinedData(encryptedIvTextBytes);

        // Generate secret key
        SecretKeySpec secretKey = generateSecretKey(password, ivParameterSpec.getIV());

        // Initialize cipher for decryption
        Cipher cipher = Cipher.getInstance(TRANSFORMATION);
        cipher.init(Cipher.DECRYPT_MODE, secretKey, ivParameterSpec);
        byte[] decryptedBytes = cipher.doFinal(encryptedBytes);

        // Convert decrypted bytes to string
        return new String(decryptedBytes, CHARSET_NAME);
    }

    /**
     * Batch decrypts the provided data.
     * Discard the data that cannot be decrypted
     *
     * @param ciphertextIndexMap Cipher text index map
     * @param password The password used to generate the encryption key.
     * @return Index Map of the Base64-encoded decrypted data.
     */
    @Override
    public Map<Integer, String> decrypt(Map<Integer, String> ciphertextIndexMap, String password) {
        Map<Integer, String> plaintextIndexMap = new HashMap<>(ciphertextIndexMap.size());
        ciphertextIndexMap.forEach((index, ciphertext) -> {
            try {
                String plaintext = decrypt(ciphertext, password);
                // Discard the data that cannot be decrypted
                if (plaintext != null && !plaintext.equals(ciphertext)) {
                    plaintextIndexMap.put(index, plaintext);
                }
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
        return plaintextIndexMap;
    }

    // Helper method to generate a random IV
    private byte[] generateRandomIv() {
        byte[] iv = new byte[IV_SIZE];
        new SecureRandom().nextBytes(iv);
        return iv;
    }

    // Helper method to combine IV and encrypted data
    private byte[] combineIvAndEncryptedData(byte[] iv, byte[] encryptedData) {
        byte[] combined = new byte[IV_SIZE + encryptedData.length];
        System.arraycopy(iv, 0, combined, 0, IV_SIZE);
        System.arraycopy(encryptedData, 0, combined, IV_SIZE, encryptedData.length);
        return combined;
    }

    // Helper method to extract IV from the combined data
    private IvParameterSpec extractIvFromCombinedData(byte[] combinedData) {
        byte[] iv = new byte[IV_SIZE];
        System.arraycopy(combinedData, 0, iv, 0, IV_SIZE);
        return new IvParameterSpec(iv);
    }

    // Helper method to extract encrypted data from the combined data
    private byte[] extractEncryptedDataFromCombinedData(byte[] combinedData) {
        int encryptedSize = combinedData.length - IV_SIZE;
        byte[] encryptedData = new byte[encryptedSize];
        System.arraycopy(combinedData, IV_SIZE, encryptedData, 0, encryptedSize);
        return encryptedData;
    }

    /**
     * Generates a SecretKeySpec for AES encryption using PBKDF2 with HMAC SHA256.
     *
     * @param password The password to derive the key from.
     * @param salt     The salt used for key derivation, typically the IV.
     * @return A SecretKeySpec based on the provided password and salt.
     * @throws Exception if an error occurs during key generation.
     */
    private SecretKeySpec generateSecretKey(String password, byte[] salt) throws Exception {
        // Use PBKDF2 for key generation
        PBEKeySpec pbeKeySpec = new PBEKeySpec(password.toCharArray(), salt, ITERATION_COUNT, KEY_SIZE);
        SecretKeyFactory keyFactory = SecretKeyFactory.getInstance(SECRET_KEY_FACTORY_ALGORITHM);
        return new SecretKeySpec(keyFactory.generateSecret(pbeKeySpec).getEncoded(), ALGORITHM);
    }
}
