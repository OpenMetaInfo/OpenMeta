package info.openmeta.framework.web.utils;

import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.AssertBusiness;
import info.openmeta.framework.orm.domain.FileObject;
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
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Optional;

/**
 * Utility class for file operations
 */
@Slf4j
public class FileUtils {

    /**
     * Gets the extension of a file.
     *
     * @param fileName fileName, supports relative or absolute paths
     * @return The file extension.
     */
    private static FileType getFileTypeByExtension(String fileName) {
        Assert.notBlank(fileName, "Filename cannot be empty!");
        try {
            String extension = FilenameUtils.getExtension(fileName);
            Assert.notBlank(extension, "File has no extension!");
            Optional<FileType> fileType = FileType.ofExtension(extension);
            return fileType.orElseThrow(() -> new BusinessException(
                    "The file {0} is not supported. Its extension is: {1}.", fileName, extension));
        } catch (Exception e) {
            throw new IllegalArgumentException("Failed to parse the extension of file: {0}!", fileName);
        }
    }

    /**
     * Gets the fileObject by path.
     *
     * @param path     The path of the file
     * @param fileName The name of the file
     * @return The fileObject with the file content
     */
    public static FileObject getFileObjectByPath(String path, String fileName) {
        Assert.notBlank(fileName, "Filename cannot be empty!");
        String fullName = path + fileName;
        ClassPathResource resource = new ClassPathResource(fullName);
        Assert.isTrue(resource.exists(), "File does not exist: {0}", fullName);
        FileObject fileObject = new FileObject();
        try (InputStream inputStream = resource.getInputStream()) {
            FileType actualFileType = getActualFileType(fullName, inputStream);
            FileType seemingFileType = getFileTypeByExtension(fullName);
            validateFileType(fullName, actualFileType, seemingFileType);
            fileObject.setFileName(fileName);
            fileObject.setFileType(actualFileType);
            String content = FileCopyUtils.copyToString(new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8)));
            fileObject.setContent(content);
        } catch (IOException e) {
            throw new IllegalArgumentException("Failed to read the content of file: {0}", fullName, e);
        }
        return fileObject;
    }

    /**
     * Validates if the file type, determined by its real mimetype, is within the acceptable range.
     *
     * @param file The MultipartFile to validate
     * @return The fileObject object
     */
    public static FileObject getFileObject(MultipartFile file) {
        FileObject fileObject = new FileObject();
        String fileName = file.getOriginalFilename();
        try (InputStream inputStream = file.getInputStream()) {
            FileType actualFileType = getActualFileType(fileName, inputStream);
            FileType seemingFileType = FileType.of(file.getContentType()).orElseThrow(() -> new BusinessException(
                    "The file {0} is not supported. Its content type is: {1}.", fileName, file.getContentType()));
            validateFileType(fileName, actualFileType, seemingFileType);
            fileObject.setFileName(fileName);
            fileObject.setFileType(actualFileType);
            String content = FileCopyUtils.copyToString(new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8)));
            fileObject.setContent(content);
            return fileObject;
        } catch (IOException e) {
            throw new BusinessException("Failed to read the uploaded file!", e);
        }
    }

    /**
     * Gets the fileName without extension.
     *
     * @param file The uploaded multipart file object
     * @return The fileName without extension
     */
    public static String getShortFileName(MultipartFile file) {
        String originalFileName = file.getOriginalFilename();
        if (originalFileName != null && originalFileName.contains(".")) {
            // Get the index of the last dot
            int dotIndex = originalFileName.lastIndexOf('.');
            return originalFileName.substring(0, dotIndex);
        }
        // If the file has no extension or the name is empty, return the original name
        return originalFileName;
    }

    /**
     * Gets the actual fileType of the uploaded file.
     *
     * @param file The uploaded file
     * @return The actual fileType
     */
    public static FileType getActualFileType(MultipartFile file) {
        String fileName = file.getOriginalFilename();
        try (InputStream inputStream = file.getInputStream()) {
            FileType actualFileType = getActualFileType(fileName, inputStream);
            FileType seemingFileType = FileType.of(file.getContentType()).orElseThrow(() -> new BusinessException(
                    "The file {0} is not supported. Its content type is: {1}.", fileName, file.getContentType()));
            validateFileType(fileName, actualFileType, seemingFileType);
            return actualFileType;
        } catch (IOException e) {
            throw new BusinessException("Failed to read the uploaded file!", e);
        }
    }

    /**
     * Gets the actual fileType of the uploaded file.
     *
     * @param fileName fileName, using relative paths
     * @return The actual fileType
     */
    private static FileType getActualFileType(String fileName, InputStream fileStream) {
        try {
            Metadata fileMetadata = new Metadata();
            fileMetadata.set("resourceName", fileName);
            String mimetype = new Tika().detect(fileStream, fileMetadata);
            Optional<FileType> actualFileType = FileType.of(mimetype);
            return actualFileType.orElseThrow( () -> new BusinessException(
                    "The file {0} is not supported. Its actual file type (MimeType) is: {1}.", fileName, mimetype));
        } catch (Throwable e) {
            throw new BusinessException("Failed to read the uploaded file!", e);
        }
    }

    /**
     * Validates if the file type, determined by its real mimetype, is within the acceptable range.
     *
     * @param fileName        The name of the file
     * @param actualFileType  The actual fileType
     * @param seemingFileType The seeming fileType
     */
    private static void validateFileType(String fileName, FileType actualFileType, FileType seemingFileType) {
        // Allow different image types and handle cases where the image extension was modified.
        if (FileType.COMPATIBLE_IMAGE_TYPE.contains(actualFileType)
                && FileType.COMPATIBLE_IMAGE_TYPE.contains(seemingFileType)) {
            return;
        }
        AssertBusiness.isTrue(actualFileType.equals(seemingFileType), """
                The file {0} with actual type {1} does not match the uploaded file type {2}. Please contact the
                system administrator if you have any questions.""", fileName, actualFileType, seemingFileType);
    }

}
