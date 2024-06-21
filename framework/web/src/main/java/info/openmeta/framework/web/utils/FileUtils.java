package info.openmeta.framework.web.utils;

import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.AssertBusiness;
import info.openmeta.framework.orm.enums.FileType;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.tika.Tika;
import org.apache.tika.metadata.Metadata;
import org.springframework.core.io.ClassPathResource;
import org.springframework.util.FileCopyUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

/**
 * Utility class for file operations
 */
@Slf4j
public class FileUtils {

    /**
     * Reads the content of a file into a string.
     *
     * @param fileName fileName, supports relative or absolute paths
     * @return The content of the file
     */
    public static String getContent(String fileName) {
        Assert.notBlank(fileName, "Filename cannot be empty!");
        ClassPathResource resource = new ClassPathResource(fileName);
        Assert.isTrue(resource.exists(), "File does not exist: {0}", fileName);
        try {
            return FileCopyUtils.copyToString(new BufferedReader(new InputStreamReader(resource.getInputStream(), StandardCharsets.UTF_8)));
        } catch (IOException e) {
            throw new IllegalArgumentException("Failed to read the content of file: {0}", fileName);
        }
    }

    /**
     * Gets the extension of a file.
     *
     * @param fileName fileName, supports relative or absolute paths
     * @return The file extension.
     */
    public static String getExtension(String fileName) {
        Assert.notBlank(fileName, "Filename cannot be empty!");
        try {
            String extension = FilenameUtils.getExtension(fileName);
            Assert.notBlank(extension, "File has no extension!");
            return extension;
        } catch (Exception e) {
            throw new IllegalArgumentException("Failed to parse the extension of file: {0}!", fileName);
        }
    }

    /**
     * Validates if the file type, determined by its real mimetype, is within the acceptable range.
     * @param file The MultipartFile to validate
     */
    public static void validFileType(MultipartFile file) {
        String mimetype;
        String fileName = file.getOriginalFilename();
        try {
            Metadata metadata = new Metadata();
            metadata.set("resourceName", fileName);
            mimetype = new Tika().detect(file.getInputStream(), metadata);
        } catch (Exception e) {
            throw new BusinessException("Failed to read the uploaded file!", e);
        }
        FileType fileType = FileType.of(mimetype);
        AssertBusiness.notNull(fileType, """
                The file {0} is not supported. Its actual file type (MimeType) is: {1}.
                Please contact the system administrator if you have any questions.""", fileName, mimetype);
        FileType contentType = FileType.of(file.getContentType());
        // Allow different image types and handle cases where the image extension was modified.
        if (FileType.COMPATIBLE_IMAGE_TYPE.contains(fileType) && FileType.COMPATIBLE_IMAGE_TYPE.contains(contentType)) {
            return;
        }
        AssertBusiness.isTrue(mimetype.equals(file.getContentType()), """
                The file {0} with actual type {1} does not match the uploaded file type {2}. Please contact the
                system administrator if you have any questions.""", fileName, mimetype, file.getContentType());
    }

    /**
     * Validates if the file types of the provided files, determined by their real mimetypes, are within the acceptable range.
     * @param files The list of MultipartFiles to validate
     */
    public static void validFileType(MultipartFile[] files) {
        Arrays.stream(files).forEach(FileUtils::validFileType);
    }
}
