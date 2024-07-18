package info.openmeta.starter.metadata.service;

import info.openmeta.starter.metadata.entity.SysConfig;
import info.openmeta.framework.orm.service.EntityService;

/**
 * SysConfig Model Service Interface
 */
public interface SysConfigService extends EntityService<SysConfig, Long> {

    /**
     * Query system config by code
     *
     * @param code config code
     */
    SysConfig getConfigByCode(String code);

}