package com.bee.platform.cloud.si.manufacture.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.PlcFactoryGatewayDTO;
import com.bee.platform.cloud.si.manufacture.entity.PlcFactoryGateway;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.FactoryGatewayParamRQ;
import com.bee.platform.cloud.si.manufacture.rq.FactoryGatewayRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * plc 工厂和华辰智通网关的配置 服务类
 *
 * @author junyang.li
 * @since 2019-10-11
 */
public interface PlcFactoryGatewayService extends IService<PlcFactoryGateway> {
    /**
     * @notes: 通过工厂id查询工厂的网关列表
     * @Author: junyang.li
     * @Date: 20:24 2019/10/11
     * @param factoryId : 当前用户工厂id
     * @param rq : 查询参数
     * @param pagination : 分页对象
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.PlcFactoryGatewayDTO>>
     */
    ResponseResult<List<PlcFactoryGatewayDTO>> getFactoryGateways(int factoryId, FactoryGatewayParamRQ rq, Pagination pagination);
    /**
     * @notes: 为当前工厂新增-华辰智通PLC数据采集网关
     * @Author: junyang.li
     * @Date: 14:41 2019/10/11
     * @param userInfo : 当前用户
     * @param rq :  新增参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> addFactoryGateways(AuthPlatformUserInfo userInfo, FactoryGatewayRQ rq);
    /**
     * @notes: 禁用或启用PLC数据采集网关
     * @Author: junyang.li
     * @Date: 14:53 2019/10/11
     * @param userInfo : 当前用户
     * @param hcGatewayId :  新增的网关id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum>updateFactoryGateways(AuthPlatformUserInfo userInfo,String hcGatewayId);
    /**
     * @notes: 删除网关
     * @Author: junyang.li
     * @Date: 14:53 2019/10/11
     * @param userInfo : 当前用户
     * @param hcGatewayId : 待删除的网关id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum>deleteFactoryGateways(AuthPlatformUserInfo userInfo,String hcGatewayId);
    /**
     * @notes: 通过网关id查询网关数据
     * @Author: junyang.li
     * @Date: 20:40 2019/10/11
     * @param hcGatewayId : 网关id
     * @return: com.bee.platform.cloud.si.manufacture.entity.PlcFactoryGateway
     */
    PlcFactoryGateway getGatewayByHcGatewayId(String hcGatewayId);
}
