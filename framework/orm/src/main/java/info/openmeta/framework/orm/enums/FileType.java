package info.openmeta.framework.orm.enums;

import com.google.common.collect.Sets;
import info.openmeta.framework.base.utils.Assert;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Map;
import java.util.Optional;
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
    XML("xml", "text/xml"),
    YAML("yaml", "application/x-yaml"),

    // Compressed file types
    ZIP("zip", "application/zip"),
    GZIP("gzip", "application/gzip"),
    TAR("tar", "application/x-tar"),
    RAR("rar", "application/vnd.rar"),
    GZ("gz", "application/gz"),

    // Audio types
    MP3("mp3", "audio/mpeg"),
    WAV("wav", "audio/x-wav"),
    AAC("aac", "audio/aac"),
    OGG("ogg", "audio/ogg"),
    FLAC("flac", "audio/flac"),

    // Video types
    MP4("mp4", "video/mp4"),
    AVI("avi", "video/x-msvideo"),
    MOV("mov", "video/quicktime"),
    WMV("wmv", "video/x-ms-wmv"),
    FLV("flv", "video/x-flv"),
    ;

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

    // Code file types
    public static final Set<FileType> CODE_TYPE = Sets.newHashSet(JSON, XML, YAML);

    /**
     * Get file type by mimeType.
     *
     * @param mimetype the mimeType of the file
     * @return Optional<FileType>
     */
    public static Optional<FileType> of(String mimetype) {
        Assert.notNull(mimetype, "Cannot process files with empty Mimetype attribute!", mimetype);
        return Optional.ofNullable(mimeTypeMap.get(mimetype));
    }

    /**
     * Get file type by file extension.
     *
     * @param extension the file extension
     * @return Optional<FileType>
     */
    public static Optional<FileType> ofExtension(String extension) {
        return Optional.ofNullable(typeMap.get(extension));
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
