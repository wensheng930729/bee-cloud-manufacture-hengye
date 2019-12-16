package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.dto.ConfigTestAttributeDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigTestAttribute;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.ConfigSearchTypeListRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigTestAttributeSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigTestAttributeSearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigTestAttributeUpdateRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;

import java.util.List;

/**
 * <p>
 * 化验属性配置表 服务类
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
public interface ConfigTestAttributeService extends IService<ConfigTestAttribute> {
    /**
     * 根据条件搜索化验属性列表
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return 化验属性列表
     */
    List<ConfigTestAttributeDTO> searchTestAttributeList(AuthPlatformUserInfo userInfo, ConfigTestAttributeSearchRQ rq);

    /**
     * 保存化验属性
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return 化验属性id
     */
    Integer saveTestAttribute(AuthPlatformUserInfo userInfo, ConfigTestAttributeSaveRQ rq);

    /**
     * 修改化验属性
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return 化验属性id
     */
    Integer updateTestAttribute(AuthPlatformUserInfo userInfo, ConfigTestAttributeUpdateRQ rq);

    /**
     * 根据id删除化验属性
     * @param userInfo 用户信息
     * @param id id
     */
    void deleteTestAttributeById(AuthPlatformUserInfo userInfo, Integer id);

    List<ConfigTestAttributeDTO> getTestAttributeByType(AuthPlatformUserInfo userInfo, List<Integer> types);
}
