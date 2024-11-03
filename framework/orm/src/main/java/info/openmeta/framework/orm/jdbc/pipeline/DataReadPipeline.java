package info.openmeta.framework.orm.jdbc.pipeline;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.jdbc.pipeline.chain.FieldProcessorFactoryChain;
import info.openmeta.framework.orm.jdbc.pipeline.factory.*;
import info.openmeta.framework.orm.jdbc.pipeline.chain.FieldProcessorChain;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Data processing pipeline for READ model data, completing the inspection and conversion of model output data.
 * Including decryption of ciphertext fields, calculation of non-storage computed fields,
 * conversion of option sets, ManyToOne/OneToOne field conversion, etc.
 * <p>
 * When processing related ManyToOne, OneToOne, OneToMany, ManyToMany fields and cascaded fields,
 * without permission check temporarily.
 */
@Slf4j
public class DataReadPipeline extends DataPipeline {

    private static final AccessType accessType = AccessType.READ;
    private final FlexQuery flexQuery;

    /**
     * Build the READ pipeline.
     * The fields to be processed are extracted from the FlexQuery.
     *
     * @param modelName model name
     * @param flexQuery flexQuery
     */
    public DataReadPipeline(String modelName, FlexQuery flexQuery) {
        super(modelName);
        this.flexQuery = flexQuery;
        // When requesting fields based on OneToMany, only the `ManyToOne` field of the associated model
        // is required for GroupBy the result of the associated model.
        this.fields = flexQuery.getFields().stream()
                .filter(field -> !(
                        ModelConstant.PRIMARY_KEYS.contains(field)
                        || ModelConstant.VIRTUAL_FIELDS.contains(field)
                        || field.equals(flexQuery.getKeepIdField())))
                .collect(Collectors.toSet());
        this.processorChain = buildFieldProcessorChain();
    }

    /**
     * Build the field processing chain according to the data processing order of the READ scenario,
     * and generate the final field processing responsibility chain `FieldProcessorChain`.
     */
    @Override
    public FieldProcessorChain buildFieldProcessorChain() {
        ConvertType convertType = flexQuery.getConvertType();
        FieldProcessorFactoryChain factoryChain = FieldProcessorFactoryChain.of(modelName, accessType)
                .addFactory(new XToOneGroupProcessorFactory(flexQuery))
                .addFactory(new NormalProcessorFactory())
                // Batch decryption processing before output the data
                .addFactory(new EncryptProcessorFactory())
                // Type conversion processing before calculation for List, MultiOption, Json, Filter fields.
                .addFactory(new TypeCastProcessorFactory(convertType))
                // Calculation processing for non-storage computed fields, after the dynamic cascaded processing.
                .addFactory(new ComputeProcessorFactory())
                // Expand processing for Boolean fields, after the processing of computed fields and cascaded fields.
                .addFactory(new ExpandProcessorFactory(convertType))
                .addFactory(new MaskingProcessorFactory())
                .addFactory(new XToManyProcessorFactory(flexQuery));
        return factoryChain.generateProcessorChain(fields);
    }

    /**
     * Process the output rows data.
     *
     * @param rows rows data
     */
    @Override
    public void processReadData(List<Map<String, Object>> rows) {
        processorChain.processOutputRows(rows);
    }

}
