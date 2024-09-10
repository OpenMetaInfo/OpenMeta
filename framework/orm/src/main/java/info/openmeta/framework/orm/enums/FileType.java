package info.openmeta.framework.orm.enums;

import com.google.common.collect.Sets;
import info.openmeta.framework.base.utils.Assert;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * File type Enum.
 * Image types: JPG, PNG, SVG, GIF, ICO
 * Document types: CSV, TXT, DOC, DOCX, PPT, PPTX, XLS, XLSX, PDF
 * Code file types: JSON, XML
 */
@Getter
@AllArgsConstructor
public enum FileType {
    // Image types. jpg and jpeg are both `image/jpeg`
    JPG("jpg", "image/jpeg"),
    PNG("png", "image/png"),
    WEBP("webp", "image/webp"),
    BMP("bmp", "image/bmp"),
    TIF("tif", "image/tiff"),
    SVG("svg", "image/svg+xml"),
    GIF("gif", "image/gif"),
    ICO("ico", "image/vnd.microsoft.icon"),

    // Document types
    CSV("csv", "text/csv"),
    TXT("txt", "text/plain"),
    DOC("doc", "application/msword"),
    DOCX("docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document"),
    PPT("ppt", "application/vnd.ms-powerpoint"),
    PPTX("pptx", "application/vnd.openxmlformats-officedocument.presentationml.presentation"),
    XLS("xls", "application/vnd.ms-excel"),
    XLSX("xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
    PDF("pdf", "application/pdf"),

    // Code file types
    JSON("json", "application/json"),
    XML("xml", "text/xml");

    private final String type;
    private final String mimetype;

    /** type map */
    static private final Map<String, FileType> typeMap = Stream.of(values()).collect(Collectors.toMap(FileType::getType, Function.identity()));

    /** mimetype map */
    static private final Map<String, FileType> mimeTypeMap = Stream.of(values()).collect(Collectors.toMap(FileType::getMimetype, Function.identity()));

    // Compatible image types
    public static final Set<FileType> COMPATIBLE_IMAGE_TYPE = Sets.newHashSet(JPG, PNG, WEBP, BMP);

    // Image types
    public static final Set<FileType> IMAGE_TYPE = Sets.newHashSet(JPG, PNG, WEBP, BMP, TIF, SVG, GIF, ICO);

    // Document types
    public static final Set<FileType> DOCUMENT_TYPE = Sets.newHashSet(CSV, TXT, DOC, DOCX, PPT, PPTX, XLS, XLSX, PDF);

    // File types
    public static final Set<FileType> FILE_TYPE = Sets.newHashSet(JPG, PNG, WEBP, BMP, TIF, SVG, GIF, ICO, CSV, TXT, DOC, DOCX, PPT, PPTX, XLS, XLSX, PDF);

    // Code file types
    public static final Set<FileType> CODE_TYPE = Sets.newHashSet(JSON, XML);

    /**
     * Get file type by mimeType.
     *
     * @param mimetype the mimeType of the file
     * @return FileType
     */
    public static FileType of(String mimetype) {
        Assert.notNull(mimetype, "Cannot process files with empty Mimetype attribute!", mimetype);
        return mimeTypeMap.get(mimetype);
    }

    /**
     * Get file type by file extension.
     *
     * @param extension the file extension
     * @return FileType
     */
    public static FileType ofExtension(String extension) {
        return typeMap.get(extension);
    }

    /**
     * Get the file extension, `.` is included.
     *
     * @return the file extension
     */
    public String getExtension() {
        return "." + type;
    }
}
