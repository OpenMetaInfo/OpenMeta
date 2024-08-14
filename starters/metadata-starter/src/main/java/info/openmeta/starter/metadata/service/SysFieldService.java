package info.openmeta.starter.metadata.service;

import info.openmeta.starter.metadata.entity.SysField;
import info.openmeta.framework.orm.service.EntityService;

/**
 * SysField Model Service Interface
 */
public interface SysFieldService extends EntityService<SysField, Long> {

    /**
     * Reload the memory cache of model metadata.
     */
    void reloadModelManager();
}