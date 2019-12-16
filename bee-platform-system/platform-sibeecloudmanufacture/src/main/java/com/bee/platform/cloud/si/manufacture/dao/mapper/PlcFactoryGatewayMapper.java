package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.FactoryGatewayParam;
import com.bee.platform.cloud.si.manufacture.entity.PlcFactoryGateway;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;

/**
 * <p>
 * plc 工厂和华辰智通网关的配置 Mapper 接口
 * </p>
 *
 * @author MP123
 * @since 2019-10-11
 */
public interface PlcFactoryGatewayMapper extends BaseMapper<PlcFactoryGateway> {
    /**
     * @notes: 从数据库中查询工厂配置的网关列表
     * @Author: junyang.li
     * @Date: 14:12 2019/10/11
     * @param param : 参数
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcFactoryGateway>
     */
    List<PlcFactoryGateway> selectFactoryGateways(FactoryGatewayParam param);
    /**
     * @notes: 从数据库中查询工厂配置的网关列表
     * @Author: junyang.li
     * @Date: 14:12 2019/10/11
     * @param param : 参数
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcFactoryGateway>
     */
    List<PlcFactoryGateway> selectFactoryGateways(FactoryGatewayParam param, Pagination pagination);
}
