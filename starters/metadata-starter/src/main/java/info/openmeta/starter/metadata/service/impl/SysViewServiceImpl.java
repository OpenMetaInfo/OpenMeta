package info.openmeta.starter.metadata.service.impl;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.metadata.entity.SysView;
import info.openmeta.starter.metadata.entity.SysViewDefault;
import info.openmeta.starter.metadata.service.SysViewDefaultService;
import info.openmeta.starter.metadata.service.SysViewService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * SysView Model Service Implementation
 */
@Service
public class SysViewServiceImpl extends EntityServiceImpl<SysView, Long> implements SysViewService {

    @Autowired
    private SysViewDefaultService viewDefaultService;

    /**
     * Set the default view for current user.
     * @param modelName Model name
     * @param viewId View ID
     * @return Boolean
     */
    @Override
    public boolean setDefaultView(String modelName, Long viewId) {
        Long currentUserId = ContextHolder.getContext().getUserId();
        Filters personalFilters = Filters.of(SysViewDefault::getModelName, Operator.EQUAL, modelName).and(SysViewDefault::getCreatedId, Operator.EQUAL, currentUserId);
        SysViewDefault defaultView = viewDefaultService.searchOne(new FlexQuery(personalFilters));
        if (defaultView != null) {
            defaultView.setViewId(viewId);
            return viewDefaultService.updateOne(defaultView);
        } else {
            defaultView = new SysViewDefault();
            defaultView.setModelName(modelName);
            defaultView.setViewId(viewId);
            defaultView.setCreatedId(currentUserId);
            return viewDefaultService.createOne(defaultView) != null;
        }
    }

    /**
     * Get the views of the specified model, including public views and personal views
     * @param modelName Model name
     * @return List of views
     */
    @Override
    public List<SysView> getModelViews(String modelName) {
        Long currentUserId = ContextHolder.getContext().getUserId();
        // Public views first, personal views second, and sorted by sequence. Search filters:
        // model_name={modelName} and (public_view=true or created_id={currentUserId}) ORDER BY public_view DESC, sequence
        Filters viewFilters = Filters.of(SysView::getModelName, Operator.EQUAL, modelName)
                .and(Filters.of(SysView::getPublicView, Operator.EQUAL, true)
                        .or(SysView::getCreatedId, Operator.EQUAL, currentUserId));
        Orders orders = Orders.ofDesc(SysView::getPublicView).addAsc(SysView::getSequence);
        List<SysView> views = this.searchList(new FlexQuery(viewFilters, orders));
        // Get the default views of current user
        Filters personalFilters = Filters.of(SysViewDefault::getModelName, Operator.EQUAL, modelName)
                .and(SysViewDefault::getCreatedId, Operator.EQUAL, currentUserId);
        SysViewDefault defaultView = viewDefaultService.searchOne(new FlexQuery(personalFilters));
        if (defaultView != null) {
            views.forEach(v -> v.setDefaultView(v.getId().equals(defaultView.getViewId())));
        }
        return views;
    }
}