package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.dto.ConfigReportFormsDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigReportFormsEnableDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigReportFormsTreeDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigReportForms;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.ConfigReportFormsUpdateStatusRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 报表配置表 服务类
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
public interface ConfigReportFormsService extends IService<ConfigReportForms> {

    /**
     * 条件查询报表配置列表
     * @param userInfo 用户信息
     * @param name 名称
     * @param page 分页对象
     * @return 报表配置列表
     */
    ResponseResult<List<ConfigReportFormsDTO>> searchReportFormsList(AuthPlatformUserInfo userInfo, String name, Page page);

    /**
     * 根据用户信息查询公司启用的报表列表
     * @param userInfo 用户信息
     * @return 报表列表
     */
    List<ConfigReportFormsEnableDTO> getEnableReportFormsList(AuthPlatformUserInfo userInfo,String type);

    /**
     * 停用报表
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return id
     */
    Integer stopReportForms(AuthPlatformUserInfo userInfo, ConfigReportFormsUpdateStatusRQ rq);

    /**
     * 启用报表
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return id
     */
    Integer enableReportForms(AuthPlatformUserInfo userInfo, ConfigReportFormsUpdateStatusRQ rq);

    /**
     * 查询报表完整树
     * @param userInfo 用户信息
     * @return 报表树
     */
    List<ConfigReportFormsTreeDTO> getReportFormsTree(AuthPlatformUserInfo userInfo);
}
