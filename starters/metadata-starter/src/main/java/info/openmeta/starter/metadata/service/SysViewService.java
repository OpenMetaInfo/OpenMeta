package info.openmeta.starter.metadata.service;

import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.metadata.entity.SysView;

import java.util.List;

/**
 * SysView Model Service Interface
 */
public interface SysViewService extends EntityService<SysView, Long> {

    /**
     * Set the default view for current user.
     * @param modelName Model name
     * @param viewId View ID
     * @return Boolean
     */
    boolean setDefaultView(String modelName, Long viewId);

    /**
     * Get the views of the specified model, including public views and personal views
     * @param modelName Model name
     * @return List of views
     */
    List<SysView> getModelViews(String modelName);
}