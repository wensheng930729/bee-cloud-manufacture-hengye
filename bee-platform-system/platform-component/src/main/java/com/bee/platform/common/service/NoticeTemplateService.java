package com.bee.platform.common.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.NoticeTemplate;
import com.bee.platform.common.enums.NoticeTemplateType;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 后台管理系统通知模板 服务类
 * </p>
 *
 * @author junyang.li123
 * @since 2019-05-06
 */
public interface NoticeTemplateService extends IService<NoticeTemplate> {

    /**
     * @notes: 通过模板类型查询模板对象
     * @Author: junyang.li
     * @Date: 16:32 2019/5/6
     * @param templateType : 模板类型
     * @return: com.bee.platform.common.entity.NoticeTemplate
     */
    NoticeTemplate getTemplateByType(NoticeTemplateType templateType);
    /**
     * @notes: 批量查询模板对象
     * @Author: junyang.li
     * @Date: 16:55 2019/5/6
     * @param templateTypes : 模板类型数组
     * @return: java.util.Map<java.lang.Integer,com.bee.platform.common.entity.NoticeTemplate>
     */
    Map<Integer,NoticeTemplate> getTemplateByTypes(NoticeTemplateType[] templateTypes);

}
