package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.dto.ConfigRepositoryDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigRepository;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.ConfigDeviceSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigDeviceUpdateRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigRepositorySaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigRepositoryUpdateRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 仓库档案 服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigRepositoryService extends IService<ConfigRepository> {
    /**
     * 查询仓库列表
     * @param userInfo 用户信息
     * @param page 分页对象
     * @return 仓库列表
     */
    ResponseResult<List<ConfigRepositoryDTO>> searchRepositoryList(AuthPlatformUserInfo userInfo,String name, Page page);

    /**
     * 保存仓库信息
     * @param userInfo 用户信息
     * @param rq 仓库信息
     * @return id
     */
    Integer saveRepository(AuthPlatformUserInfo userInfo, ConfigRepositorySaveRQ rq);

    /**
     * 修改仓库信息
     * @param userInfo 用户信息
     * @param rq 仓库信息
     * @return id
     */
    Integer updateRepository(AuthPlatformUserInfo userInfo, ConfigRepositoryUpdateRQ rq);

    /**
     * 根据id删除仓库
     * @param userInfo 用户信息
     * @param id id
     */
    void deleteRepositoryById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 根据仓库类别查询仓库列表
     * @param userInfo 用户信息
     * @param types 类别列表 (仓库类别(0 成品 1 原料 2 配料  3五金 4其他) 空为查询全部)
     * @return 仓库列表
     */
    List<ConfigRepositoryDTO> getRepositoryListByType(AuthPlatformUserInfo userInfo, List<Integer> types);

    ConfigRepositoryDTO getRepositoryById(AuthPlatformUserInfo userInfo, Integer id);
    /**
     * @notes: 查询当前用户可访问的仓库详细，暂时只返回 id 和 name 字段
     * @Author: junyang.li
     * @Date: 10:39 2019/11/26
     * @param userInfo : 当前操作人
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.ConfigRepository>
     */
    List<ConfigRepository> getRepositoryList(AuthPlatformUserInfo userInfo);
}
