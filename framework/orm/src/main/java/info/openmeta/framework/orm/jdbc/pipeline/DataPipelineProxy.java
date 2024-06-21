package info.openmeta.framework.orm.jdbc.pipeline;

import info.openmeta.framework.orm.annotation.SkipPermissionCheck;
import info.openmeta.framework.orm.domain.FlexQuery;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

/**
 * Data Pipeline Proxy for processing model data in AOP.
 */
@Component
public class DataPipelineProxy {

    /**
     * Process the rows data for Read.
     *
     * @param modelName    model name
     * @param flexQuery    flexQuery
     * @param rows      rows data
     */
    @SkipPermissionCheck
    public void processReadData(String modelName, FlexQuery flexQuery, List<Map<String, Object>> rows) {
        DataReadPipeline dataPipeline = new DataReadPipeline(modelName, flexQuery);
        dataPipeline.processReadData(rows);
    }

}
