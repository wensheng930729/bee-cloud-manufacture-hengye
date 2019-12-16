package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.entity.ConfigRepository;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 仓库档案 Mapper 接口
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigRepositoryMapper extends BaseMapper<ConfigRepository> {

    /**
     * @notes: 查询当前用户可访问的仓库详细，暂时只返回 id 和 name 字段
     * @Author: junyang.li
     * @Date: 10:39 2019/11/26
     * @param factoryId : 工厂id
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.ConfigRepository>
     */
    List<ConfigRepository> getRepositoryList(@Param("factoryId") Integer factoryId);
}
