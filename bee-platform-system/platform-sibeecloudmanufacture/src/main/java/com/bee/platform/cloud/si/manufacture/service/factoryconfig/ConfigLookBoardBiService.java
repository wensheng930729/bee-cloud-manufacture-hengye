package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.dto.ConfigLookBoardBiDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigLookBoardBiEnableDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigLookBoardBiTreeDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigLookBoardBi;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.ConfigLookBoardBiUpdateStatusRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 看板BI配置表 服务类
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
public interface ConfigLookBoardBiService extends IService<ConfigLookBoardBi> {
    /**
     * 条件查询看板BI配置
     * @param userInfo 用户信息
     * @param name 看板名称
     * @param page 分页对象
     * @return 看板BI配置列表
     */
    ResponseResult<List<ConfigLookBoardBiDTO>> searchLookBoardBiList(AuthPlatformUserInfo userInfo, String name, Page page);

    /**
     * 停用看板BI配置
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return id
     */
    Integer stopLookBoardBi(AuthPlatformUserInfo userInfo, ConfigLookBoardBiUpdateStatusRQ rq);

    /**
     * 启用看板BI配置
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return id
     */
    Integer enableLookBoardBi(AuthPlatformUserInfo userInfo, ConfigLookBoardBiUpdateStatusRQ rq);

    /**
     * 根据登陆信息查询用户企业启用的看板BI配置
     * @param userInfo 用户信息
     * @return 看板BI列表
     */
    List<ConfigLookBoardBiEnableDTO> getEnableLookBoardBiList(AuthPlatformUserInfo userInfo, String type);

    /**
     * 查询企业启用的看板BI完整树
     * @param userInfo 用户信息
     * @return BI完整树
     */
    List<ConfigLookBoardBiTreeDTO> getLookBoardBiTree(AuthPlatformUserInfo userInfo);
}
