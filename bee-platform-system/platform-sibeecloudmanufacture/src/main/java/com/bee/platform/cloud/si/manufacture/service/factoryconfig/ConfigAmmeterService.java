package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.dto.ConfigAmmeterDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigAmmeter;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.ConfigAmmeterSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigAmmeterUpdateRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 电表档案 服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigAmmeterService extends IService<ConfigAmmeter> {
    /**
     * 根据电表名称搜索电表列表
     * @param userInfo 用户信息
     * @param ammeterName 电表名称
     * @param page 分页对象
     * @return 电表列表
     */
    ResponseResult<List<ConfigAmmeterDTO>> searchAmmeterList(AuthPlatformUserInfo userInfo, String ammeterName, Page page);

    /**
     * 保存电表信息
     * @param userInfo 用户信息
     * @param rq 电表信息
     * @return 电表id
     */
    Integer saveAmmeter(AuthPlatformUserInfo userInfo, ConfigAmmeterSaveRQ rq);

    /**
     * 修改电表信息
     * @param userInfo 用户信息
     * @param rq 电表信息
     * @return 电表id
     */
    Integer updateAmmeter(AuthPlatformUserInfo userInfo, ConfigAmmeterUpdateRQ rq);

    /**
     * 根据id删除电表信息
     * @param userInfo 用户信息
     * @param id id
     */
    void deleteAmmeterById(AuthPlatformUserInfo userInfo, Integer id);

    ConfigAmmeterDTO getAmmeterById(AuthPlatformUserInfo userInfo, Integer id);

    List<ConfigAmmeterDTO> getAmmeterList(AuthPlatformUserInfo userInfo);
}
