package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * <p>
 * plc字段相关的配置 Mapper 接口
 * </p>
 *
 * @author MP123
 * @since 2019-10-11
 */
public interface PlcFieldConfigMapper extends BaseMapper<PlcFieldConfig> {
    /**
     * @notes: 查询工厂配置的plc字段
     * @Author: junyang.li
     * @Date: 15:20 2019/10/11
     * @param factoryId : plcId
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig>
     */
    List<PlcFieldConfig> selectFieldsByFactory(int factoryId);
    /**
     * @notes: 查询工厂配置的plc字段
     * @Author: junyang.li
     * @Date: 15:21 2019/10/11
     * @param factoryId : 工厂id
     * @param pagination : 分页对象
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig>
     */
    List<PlcFieldConfig> selectFieldsByFactory(int factoryId, Pagination pagination);
    /**
     * @notes: 从数据库中查询是否已存在，并返回不存在的字段
     * @Author: junyang.li
     * @Date: 16:02 2019/10/11
     * @param plcId :
     * @param set :
     * @return: java.util.List<java.lang.String>
     */
    Set<String> selectNotExistField(@Param("plcId")int plcId,@Param("data") Set<String> set);
    /**
     * @notes: 批量插入
     * @Author: junyang.li
     * @Date: 16:33 2019/10/11
     * @param configs : 目标数据
     * @return: void
     */
    void insertAll(@Param("data") List<PlcFieldConfig> configs);
    /**
     * @notes:  查询所有
     * @Author: junyang.li
     * @Date: 13:21 2019/10/21
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig>
     */
    List<PlcFieldConfig> selectAll();
}
