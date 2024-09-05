package info.openmeta.starter.metadata.service.impl;

import com.fasterxml.jackson.core.type.TypeReference;
import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.exception.SystemException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.Cast;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.enums.FileType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.framework.orm.utils.LambdaUtils;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.framework.web.utils.FileUtils;
import info.openmeta.starter.metadata.entity.SysPreData;
import info.openmeta.starter.metadata.service.SysPreDataService;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.Serializable;
import java.io.StringReader;
import java.util.*;
import java.util.stream.Collectors;

import static info.openmeta.framework.orm.constant.ModelConstant.ID;

/**
 * SysPreData Model Service Implementation
 * Predefined data: model + preId as a unique identifier, used to bind model row ID, thus preId is unique within the model.
 * Among them, ManyToOne and OneToOne fields directly reference preId, ManyToMany fields reference a list of preIds,
 * OneToMany fields support a data list, where the data in the list does not need to declare the main model's preId
 * but must declare the relatedModel's preId.
 */
@Service
public class SysPreDataServiceImpl extends EntityServiceImpl<SysPreData, Long> implements SysPreDataService {

    /**
     * Load the specified list of predefined data files from the root directory: resources/data.
     * Supports data files in JSON, XML, and CSV formats. Data files support a two-layer domain model,
     * i.e., main model and subModel, but they will be created separately when loading.
     * The main model is created first to generate the main model id, then the subModel data is created.
     *
     * @param fileNames List of relative directory data file names to load
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void loadPredefinedData(List<String> fileNames) {
        String dataDir = BaseConstant.PREDEFINED_DATA_DIR;
        for (String fileName : fileNames) {
            FileInfo fileInfo = FileUtils.getFileInfoByPath(dataDir, fileName);
            loadFileInfo(fileInfo);
        }
    }

    /**
     * Loads predefined data from a given multipart file.
     * This method processes the provided multipart file to load predefined data into the system.
     * The file is expected to be in a format that is recognized by the implementation, such as CSV, JSON, or XML.
     *
     * @param file the multipart file containing the predefined data to be loaded into the system.
     *             The file should not be null and must contain valid data as per the required format.
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void loadPredefinedData(MultipartFile file) {
        FileInfo fileInfo = FileUtils.getFileInfo(file);
        loadFileInfo(fileInfo);
    }

    /**
     * Load file information and process different data formats based on the file type.
     *
     * @param fileInfo File information object
     */
    private void loadFileInfo(FileInfo fileInfo) {
        if (StringUtils.isBlank(fileInfo.getContent())) {
            return;
        }
        if (FileType.JSON.equals(fileInfo.getFileType())) {
            processJson(fileInfo.getContent());
        } else if (FileType.XML.equals(fileInfo.getFileType())) {
            processXml(fileInfo.getContent());
        } else if (FileType.CSV.equals(fileInfo.getFileType())) {
            // Treats the first part of the file name as the model name
            String fileName = fileInfo.getFileName();
            String modelName = fileName.substring(0, fileName.indexOf('.')).trim();
            Assert.isTrue(ModelManager.existModel(modelName),
                    "Model {0} specified in the fileName `{1}` does not exist!", modelName, fileName);
            processCsv(modelName, fileInfo.getContent());
        } else {
            throw new IllegalArgumentException("Unsupported file type for predefined data: {0}", fileInfo.getFileType());
        }
    }

    /**
     * Process JSON format data, parse and map according to the data order in the JSON text to LinkedHashMap,
     * and process in order. JSON format data supports two-layer model nesting, i.e., main model and relatedModel,
     * but they will be created separately when loading. The main model data is created first to generate the ID,
     * then the relatedModel data is created.
     * <p>
     *     Data under the JSON model supports two formats:
     *     <ul>
     *         <li>Single Map format: { model1: {field1: value1, field2: value2, ...}, model2: {...}, ...}</li>
     *         <li>List<Map> format: { model1: [{field1: value1, field2: value2}, {...}], model2: {...}, ...}</li>
     * </p>
     *
     * @param content JSON string data content
     */
    private void processJson(String content) {
        Map<String, Object> predefinedData = JsonMapper.stringToObject(content, new TypeReference<LinkedHashMap<String, Object>>() {});
        predefinedData.forEach(this::processModelData);
    }

    private void processXml(String content) {
        // TODO: Process the data of XML format
    }

    /**
     * Process CSV format data, parse and map according to the data order in the CSV text.
     * The first line of the CSV content is the header, and the data content is converted to a list of map.
     * Default separator is comma, and the quote character is double quote
     *
     * @param modelName Model name
     * @param content CSV string data content
     */
    private void processCsv(String modelName, String content) {
        // Parse the CSV content using CSVParser, automatically detect and skip the first line as the header
        CSVFormat csvFormat = CSVFormat.Builder.create()
                .setHeader()
                .setSkipHeaderRecord(true)
                .build();
        // Parse the CSV content using CSVParser
        CSVParser parser;
        try {
            parser = csvFormat.parse(new StringReader(content));
        } catch (IOException e) {
            throw new SystemException("Failed to parse the CSV content: {0}", e.getMessage());
        }
        // Get the header map of the CSV content
        Map<String, Integer> headerMap = parser.getHeaderMap();
        List<Map<String, Object>> csvDataList = new ArrayList<>();
        // Iterate over each record in the CSV content, not including the header
        for (CSVRecord record : parser) {
            Map<String, Object> rowData = new HashMap<>();
            for (Map.Entry<String, Integer> header : headerMap.entrySet()) {
                String fieldName = header.getKey().trim();
                Assert.notBlank(fieldName, "The field name in the CSV header cannot be empty!");
                String stringValue = record.get(header.getValue()).trim();
                FieldType fieldType = ModelManager.getModelField(modelName, fieldName).getFieldType();
                if (ModelConstant.ID.equals(fieldName) || FieldType.TO_ONE_TYPES.contains(fieldType)) {
                    // Retain the preID of ID, ManyToOne, and OneToOne fields, which are String value.
                    rowData.put(fieldName, stringValue);
                } else {
                    Object fieldValue = FieldType.convertStringToObject(fieldType, stringValue);
                    rowData.put(fieldName, fieldValue);
                }
            }
            csvDataList.add(rowData);
        }
        // Process the model data list
        processModelData(modelName, csvDataList);
    }

    /**
     * Process the model predefined data, which can be a single Map or a List<Map> format.
     *
     * @param model Model name
     * @param predefinedData Predefined data
     */
    private void processModelData(String model, Object predefinedData) {
        ModelManager.validateModel(model);
        if (predefinedData instanceof List) {
            ((List<?>) predefinedData).forEach(row -> {
                if (row instanceof Map) {
                    handlePredefinedData(model, Cast.of(row));
                } else {
                    throw new IllegalArgumentException("When defining model data in List structure, " +
                            "the internal data only supports Map format {0}: {1}", model, predefinedData);
                }
            });
        } else if (predefinedData instanceof Map) {
            handlePredefinedData(model, Cast.of(predefinedData));
        } else {
            throw new IllegalArgumentException(
                    "Model predefined data only supports Map or List<Map> format {0}: {1}", model, predefinedData);
        }
    }

    /**
     * Load a predefined data record.
     * If there is predefined data for OneToMany and ManyToMany fields, recursively load the sub-table data,
     * using an ordered Map to ensure that the data processing order is consistent with the file definition.
     * When the OneToMany field value is empty, it indicates the deletion of existing associated model data.
     *
     * @param model Model name
     * @param row Predefined data record
     */
    private Long handlePredefinedData(String model, Map<String, Object> row) {
        Map<String, Object> oneToManyMap = new LinkedHashMap<>();
        // Extract OneToMany fields contained in the predefined data and put them in the OneToManyMap,
        // and then remove them from row, used to independently handle the creation or update of associated model.
        Set<String> oneToManyFields = row.keySet().stream()
                .filter(field -> FieldType.ONE_TO_MANY.equals(ModelManager.getModelField(model, field).getFieldType()))
                .collect(Collectors.toSet());
        oneToManyFields.forEach(field -> oneToManyMap.put(field, row.remove(field)));
        // Load main model data
        Long rowId = createOrUpdateData(model, row);
        // Load OneToMany data
        loadOneToManyRows(model, rowId, oneToManyMap);
        return rowId;
    }

    /**
     * Load OneToMany field data
     * Based on and retain the existing Many side ids, delete Many side data that does not exist in the predefined data file.
     *
     * @param model Main model name
     * @param mainId Main model row ID
     * @param oneToManyMap OneToMany { fieldName: data list} mapping relationship, the value must be a list type.
     */
    private void loadOneToManyRows(String model, Serializable mainId, Map<String, Object> oneToManyMap) {
        oneToManyMap.forEach((field, rows) -> {
            if (!(rows instanceof Collection)) {
                throw new IllegalArgumentException("The data of OneToMany field {0}:{1} must be a list: {2}", model, field, rows);
            }
            MetaField oneToManyMetaField = ModelManager.getModelField(model, field);
            // Process each item in rows, ensuring each is a Map
            List<Long> manyIds = ((Collection<?>) rows).stream()
                    .peek(item -> {
                        if (!(item instanceof Map)) {
                            throw new IllegalArgumentException(
                                    "The single predefined data of the OneToMany field {0}:{1} must be in Map format: {2}",
                                    model, field, item);
                        }
                    })
                    .map(item -> {
                        Map<String, Object> castedItem = Cast.of(item);
                        castedItem.put(oneToManyMetaField.getRelatedField(), mainId);
                        // Load OneToMany single row data
                        return handlePredefinedData(oneToManyMetaField.getRelatedModel(), castedItem);
                    })
                    .toList();
            // Delete Many side data, but retain those that appear in the predefined data file.
            Filters deleteFilters = Filters.eq(oneToManyMetaField.getRelatedField(), mainId);
            if (!manyIds.isEmpty()) {
                deleteFilters.andNotIn(ID, manyIds);
            }
            modelService.deleteByFilters(oneToManyMetaField.getRelatedModel(), deleteFilters);
        });
    }

    /**
     * Determine whether to create or update predefined data based on whether the main model preId already exists.
     *
     * @param model Model name
     * @param row Predefined data record
     * @return Record ID created or updated
     */
    private Long createOrUpdateData(String model, Map<String, Object> row) {
        SysPreData preData = getPreDataByPreId(model, row);
        if (preData != null && Boolean.TRUE.equals(preData.getFrozen())) {
            // The current data is frozen, and the data ID is returned directly
            return preData.getRowId();
        }
        // Replace the preID of ManyToOne, OneToOne, and ManyToMany fields with the row ID
        this.replaceReferencedPreIds(model, row);
        if (preData == null) {
            // Create the data and return the data ID
            String preId = (String) row.get(ID);
            row.remove(ID);
            Long rowId = modelService.createOne(model, row);
            generatePreData(model, preId, rowId);
            return rowId;
        } else {
            // Update the data and return the data ID
            row.put(ID, preData.getRowId());
            // Clear other fields that do not appear in the predefined data
            Set<String> updatableStoredFields = ModelManager.getModelUpdatableFieldsWithoutXToMany(model);
            updatableStoredFields.removeAll(row.keySet());
            updatableStoredFields.forEach(field -> row.put(field, null));
            boolean result =  modelService.updateOne(model, row);
            if (!Boolean.TRUE.equals(result)) {
                boolean isExist = modelService.exist(model, preData.getRowId());
                Assert.isTrue(isExist, "Updating predefined data for model {0} ({1}) failed " +
                        "as it has already been physically deleted!", model, preData.getRowId());
            }
            return preData.getRowId();
        }
    }

    /**
     * Get the SysPreData object by preID.
     * @param model Model name
     * @param row Predefined data record
     * @return SysPreData object
     */
    private SysPreData getPreDataByPreId(String model, Map<String, Object> row) {
        Assert.isTrue(row.containsKey(ID), "Predefined data for model {0} must include the preID: {1}", model, row);
        Object preId = row.get(ID);
        Assert.isTrue(preId instanceof String, "Model {0} predefined data's preId must be of type String: {1}", model, preId);
        Filters filters = Filters.eq(SysPreData::getModel, model).andEq(SysPreData::getPreId, preId);
        return this.searchOne(new FlexQuery(filters));
    }

    /**
     * Replace the pre-defined ID of ManyToOne, OneToOne, and ManyToMany fields with the row ID.
     *
     * @param model Model name
     * @param row Predefined data record
     */
    private void replaceReferencedPreIds(String model, Map<String, Object> row) {
        for (Map.Entry<String, Object> entry : row.entrySet()) {
            if (entry.getValue() == null) {
                continue;
            }
            MetaField metaField = ModelManager.getModelField(model, entry.getKey());
            if (FieldType.TO_ONE_TYPES.contains(metaField.getFieldType()) &&
                    !(entry.getValue() instanceof Long || entry.getValue() instanceof Integer)) {
                Assert.isTrue(entry.getValue() instanceof String,
                        "Model {0} field {1}:{2} preID must be of type String: {3}",
                        model, entry.getKey(), metaField.getFieldType().getType(), entry.getValue());
                Long rowId = this.getRowIdByPreId(metaField.getRelatedModel(), Cast.of(entry.getValue()));
                entry.setValue(rowId);
            } else if (FieldType.MANY_TO_MANY.equals(metaField.getFieldType())) {
                Assert.isTrue(entry.getValue() instanceof Collection,
                        "Model {0} predefined data's {1} ManyToMany field value must be a list or empty",
                        model, entry.getKey());
                if (!CollectionUtils.isEmpty((Collection<?>) entry.getValue())) {
                    List<String> preIds = Cast.of(entry.getValue());
                    List<Long> rowIds = this.getRowIdsByPreIds(metaField.getRelatedModel(), preIds);
                    entry.setValue(rowIds);
                }
            }
        }
    }

    /**
     * Get the model row ID bound by preId.
     * @param model Model name
     * @param preId Predefined ID
     * @return Model row ID
     */
    private Long getRowIdByPreId(String model, String preId) {
        Filters filters = Filters.eq(SysPreData::getModel, model).andEq(SysPreData::getPreId, preId);
        String rowIdField = LambdaUtils.getAttributeName(SysPreData::getRowId);
        List<Long> rowIds = this.getRelatedIds(filters, rowIdField);
        Assert.notEmpty(rowIds, "The preID of the predefined data for model {0}: {1} does not exist " +
                "in the predefined data table and may not have been created yet!", model, preId);
        return rowIds.getFirst();
    }

    /**
     * Get a list of model row IDs bound by preIds.
     * @param model Model name
     * @param preIds Predefined IDs
     * @return List of model row IDs
     */
    private List<Long> getRowIdsByPreIds(String model, List<String> preIds) {
        Filters filters = Filters.eq(SysPreData::getModel, model).andIn(SysPreData::getPreId, preIds);
        String rowIdField = LambdaUtils.getAttributeName(SysPreData::getRowId);
        List<Long> rowIds = this.getRelatedIds(filters, rowIdField);
        Assert.notEmpty(rowIds, "The preIDs of the predefined data for model {0}: {1} do not exist " +
                "in the predefined data table and may not have been created yet!", model, preIds);
        return rowIds;
    }

    /**
     * Create predefined data and bind the model row ID.
     *
     * @param model Model name
     * @param preId Predefined ID
     * @param rowId Model record ID
     */
    private void generatePreData(String model, String preId, Long rowId) {
        SysPreData preData = new SysPreData();
        preData.setModel(model);
        preData.setPreId(preId);
        preData.setRowId(rowId);
        this.createOne(preData);
    }
}