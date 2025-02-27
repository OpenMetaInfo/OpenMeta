package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.domain.FileInfo;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.utils.ReflectTool;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * File and MultiFile fields group processor.
 * Query from the fileRecords for one time, and then process the File and MultiFile fields in a batch.
 */
@Slf4j
public class FilesGroupProcessor extends BaseProcessor {

    private final List<MetaField> fileFields = new ArrayList<>(0);

    /**
     * Constructor of the File and MultiFile fields group processor.
     *
     * @param firstField the first file field
     * @param accessType access type
     */
    public FilesGroupProcessor(MetaField firstField, AccessType accessType) {
        super(firstField, accessType);
        this.fileFields.add(firstField);
    }

    /**
     * Add a File or MultiFile field to the group processor.
     *
     * @param fileField File or MultiFile field
     */
    public void addFileField(MetaField fileField) {
        this.fileFields.add(fileField);
    }

    /**
     * No need to process the input data of the File and MultiFile fields group.
     *
     * @param rows Data collection
     */
    public void batchProcessInputRows(List<Map<String, Object>> rows) {
    }

    /**
     * Batch process the output data of the File and MultiFile fields group.
     *
     * @param rows The list of output data
     */
    public void batchProcessOutputRows(List<Map<String, Object>> rows) {
        List<String> fileIds = getFileIds(rows);
        if (CollectionUtils.isEmpty(fileIds)) {
            return;
        }
        List<FileInfo> fileInfos = ReflectTool.getModelFiles(modelName, fileIds);
        Map<String, FileInfo> fileInfoMap = fileInfos.stream()
                .collect(Collectors.toMap(FileInfo::getFileId, fileInfo -> fileInfo));
        for (Map<String, Object> row : rows) {
            for (MetaField fileField : fileFields) {
                String fieldName = fileField.getFieldName();
                if (FieldType.FILE.equals(fileField.getFieldType()) &&
                        StringUtils.isNotBlank((String) row.get(fieldName))) {
                    row.put(fieldName, fileInfoMap.get((String) row.get(fieldName)));
                } else if (FieldType.MULTI_FILE.equals(fileField.getFieldType()) &&
                        row.get(fieldName) instanceof List<?> fileIdList) {
                    List<FileInfo> fileInfoList = fileIdList.stream()
                            .map(fileId -> fileInfoMap.get((String) fileId))
                            .filter(Objects::nonNull)
                            .toList();
                    row.put(fieldName, fileInfoList);
                }
            }
        }
    }

    /**
     * Get the fileIds from the rows.
     *
     * @param rows The list of rows
     * @return The list of fileIds
     */
    private List<String> getFileIds(List<Map<String, Object>> rows) {
        List<String> fileIds = new ArrayList<>();
        for (Map<String, Object> row : rows) {
            fileFields.forEach(fileField -> {
                Object fileId = row.get(fileField.getFieldName());
                if (FieldType.FILE.equals(fileField.getFieldType()) &&
                        StringUtils.isNotBlank((String) fileId)) {
                    fileIds.add((String) fileId);
                } else if (FieldType.MULTI_FILE.equals(fileField.getFieldType()) &&
                        fileId instanceof List<?> fileIdList) {
                    fileIdList.forEach(id -> fileIds.add((String) id));
                }
            });
        }
        return fileIds;
    }
}
