package info.openmeta.starter.metadata.service;

import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.metadata.entity.SysPreData;

import java.util.List;

/**
 * SysPreData Model Service Interface
 */
public interface SysPreDataService extends EntityService<SysPreData, Long> {

    /**
     * Load the specified list of predefined data files from the root directory: resources/data.
     * Supports data files in JSON, XML, and CSV formats. Data files support a two-layer domain model,
     * i.e., main model and subModel, but they will be created separately when loading.
     * The main model is created first to generate the main model id, then the subModel data is created.
     *
     * @param fileNames List of relative directory data file names to load
     */
    void loadPredefinedData(List<String> fileNames);
}