package info.openmeta.framework.orm.domain;

import info.openmeta.framework.orm.enums.ValueType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.utils.ListUtils;
import info.openmeta.framework.orm.utils.NumberUtils;
import lombok.Data;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.stream.Collectors;


/**
 * PivotTable Object
 * The result is a two-dimensional structured data that is transposed and expanded.
 * groupBy: [a, b]
 * splitBy: [x, y]
 * Then all aggregation cases include the following 9 cases:
 *          |   0       x       x,y
 *  --------|------------------------
 *    0     |   0       x       x,y
 *    a     |   a       a,x     a,x,y
 *    a,b   |   a,b     a,b,x   a,b,x,y
 *
 */
@Data
public class PivotTable {
    private Map<String, Object> summary = new HashMap<>();
    // Aggregated operation + transposed flat data
    private List<Map<String, Object>> rows = new ArrayList<>();

    /**
     * Groups the Map list according to the value of the specified field
     * @param rows data list
     * @param field field name
     * @return Grouping results with field values as keys
     */
    public static Map<Object, List<Map<String, Object>>> groupBy(List<Map<String, Object>> rows, String field) {
        return rows.stream().collect(Collectors.groupingBy(row -> row.get(field)));
    }

    /**
     * Aggregate the searchMap query results
     * @param modelName model name
     * @param rows original data list, which has been grouped by 'groupBy'
     * @param groupByFields row grouping field list
     * @param splitByFields column grouping field list
     * @return PivotTable
     */
    public static PivotTable aggregateOperation(String modelName, List<Map<String, Object>> rows,
                                                List<String> groupByFields, List<String> splitByFields) {
        PivotTable pivotTable = new PivotTable();
        if (CollectionUtils.isEmpty(rows)) {
            return pivotTable;
        }
        // To perform aggregate calculation for numeric fields, remove the fields that appear in
        // `groupBy` and `splitBy` parameters to avoid aggregate calculation of the group fields.
        Set<String> numericFields = ModelManager.getModelNumericFields(modelName);
        numericFields.removeAll(new HashSet<>(groupByFields));
        numericFields.removeAll(new HashSet<>(splitByFields));
        Map<String, ValueType> numericFieldValueTypes = numericFields.stream()
                .map(field -> ModelManager.getModelField(modelName, field))
                .collect(Collectors.toMap(MetaField::getFieldName, f -> ValueType.of(f.getFieldType())));
        // Total the 'count' calculation as part of the PivotTable aggregation calculation.
        numericFieldValueTypes.put("count", ValueType.LONG);

        // If the groupByFields and splitByFields are empty, the summary is the sum of all numeric fields
        if (CollectionUtils.isEmpty(groupByFields) && CollectionUtils.isEmpty(splitByFields)) {
            pivotTable.setSummary(NumberUtils.sumNumericFields(rows, numericFieldValueTypes));
        }
        // pivotTable.setRows(rows);
        // TODO: Transpose the row data according to the value of the splitByFields field
        for (String field : groupByFields) {
            Map<Object, List<Map<String, Object>>> result = ListUtils.groupBy(rows, field);
        }
        return pivotTable;
    }

}
