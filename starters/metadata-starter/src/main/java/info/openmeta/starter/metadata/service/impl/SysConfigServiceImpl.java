package info.openmeta.starter.metadata.service.impl;

import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.metadata.entity.SysConfig;
import info.openmeta.starter.metadata.service.SysConfigService;
import org.springframework.stereotype.Service;

/**
 * SysConfig Model Service Implementation
 */
@Service
public class SysConfigServiceImpl extends EntityServiceImpl<SysConfig, Long> implements SysConfigService {

    /**
     * Query system config by code
     *
     * @param code config code
     */
    @Override
    public SysConfig getConfigByCode(String code) {
        return this.searchOne(new FlexQuery(Filters.of(SysConfig::getCode, Operator.EQUAL, code)));
    }

}