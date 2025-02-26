package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.DataFileInfo;
import info.openmeta.framework.orm.domain.FileInfo;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.utils.ReflectTool;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
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
        List<String> fileFieldNames = fileFields.stream().map(MetaField::getFieldName).toList();
        List<Serializable> ids = rows.stream()
                .map(r -> (Serializable) r.get(ModelConstant.ID))
                .toList();
        List<DataFileInfo> dataFileInfos = ReflectTool.getRowFiles(modelName, ids, fileFieldNames);
        Map<Serializable, Map<String, List<FileInfo>>> fileInfoMap = groupByIdAndField(dataFileInfos);
        for (Map<String, Object> row : rows) {
            Serializable id = (Serializable) row.get(ModelConstant.ID);
            for (MetaField fileField : fileFields) {
                String fieldName = fileField.getFieldName();
                if (!fileInfoMap.containsKey(id) || !fileInfoMap.get(id).containsKey(fieldName)) {
                    row.put(fieldName, null);
                    continue;
                }
                if (FieldType.FILE.equals(fileField.getFieldType())) {
                    row.put(fieldName, fileInfoMap.get(id).get(fieldName).getFirst());
                } else {
                    row.put(fieldName, fileInfoMap.get(id).get(fieldName));
                }
            }
        }
    }

    /**
     * Group the fileInfo by rowId and fieldName.
     * One file field may have multiple files according to the fieldType.
     *
     * @param dataFileInfos The list of dataFileInfos
     * @return The map of fileInfo grouped by rowId and fieldName
     */
    private Map<Serializable, Map<String, List<FileInfo>>> groupByIdAndField(List<DataFileInfo> dataFileInfos) {
        return dataFileInfos.stream()
                .collect(Collectors.groupingBy(DataFileInfo::getRowId,
                        Collectors.groupingBy(DataFileInfo::getFieldName,
                                Collectors.mapping(DataFileInfo::getFileInfo, Collectors.toList()))));
    }

}
