package info.openmeta.starter.metadata.service;

import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.metadata.entity.SysPreData;
import org.springframework.web.multipart.MultipartFile;

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

    /**
     * Loads predefined data from a given multipart file.
     * <p>
     * This method processes the provided multipart file to load predefined data
     * into the system. The file is expected to be in a format that is recognized
     * by the implementation, such as CSV, JSON, or XML.
     * </p>
     * <p>
     * The method performs necessary validations and error handling to ensure the
     * data integrity and consistency. Any issues encountered during the file
     * processing are logged appropriately, and relevant exceptions are thrown to
     * inform the caller about the specific problems.
     * </p>
     *
     * @param file the multipart file containing the predefined data to be loaded
     *             into the system. The file should not be null and must contain
     *             valid data as per the required format.
     */
    void loadPredefinedData(MultipartFile file);

}