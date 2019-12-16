package com.bee.platform.cloud.si.manufacture.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.FactoryGatewayParam;
import com.bee.platform.cloud.si.manufacture.dto.PlcFactoryGatewayDTO;
import com.bee.platform.cloud.si.manufacture.entity.PlcFactoryGateway;
import com.bee.platform.cloud.si.manufacture.dao.mapper.PlcFactoryGatewayMapper;
import com.bee.platform.cloud.si.manufacture.rq.FactoryGatewayParamRQ;
import com.bee.platform.cloud.si.manufacture.rq.FactoryGatewayRQ;
import com.bee.platform.cloud.si.manufacture.service.PlcFactoryGatewayService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.PageUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * plc 工厂和华辰智通网关的配置 服务实现类
 *
 * @author junyang.li
 * @since 2019-10-11
 */
@Service
public class PlcFactoryGatewayServiceImpl extends ServiceImpl<PlcFactoryGatewayMapper, PlcFactoryGateway> implements PlcFactoryGatewayService {

    @Autowired
    private PlcFactoryGatewayMapper plcFactoryGatewayMapper;
    /**
     * @notes: 通过工厂id查询工厂的网关列表
     * @Author: junyang.li
     * @Date: 20:24 2019/10/11
     * @param factoryId : 当前用户工厂id
     * @param rq : 查询参数
     * @param pagination : 分页对象
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.PlcFactoryGatewayDTO>>
     */
    @Override
    public ResponseResult<List<PlcFactoryGatewayDTO>> getFactoryGateways(int factoryId, FactoryGatewayParamRQ rq, Pagination pagination) {
        //查询工厂配置的网关
        FactoryGatewayParam param=new FactoryGatewayParam(factoryId,rq.getHcGatewayName(),rq.getStatus());
        List<PlcFactoryGateway> list=this.selectFactoryGateways(param,pagination);
        //判空
        if(CollectionUtils.isEmpty(list)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new ArrayList<>(),
                    PageUtils.transToPage(pagination));
        }
        //遍历
        List<PlcFactoryGatewayDTO> items=list.stream().map(obj->{
            return new PlcFactoryGatewayDTO(obj.getHcGatewayId(),obj.getHcGatewayName(),obj.getStatus());
        }).collect(Collectors.toList());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,items,
                PageUtils.transToPage(pagination));
    }
    /**
     * @notes: 为当前工厂新增-华辰智通PLC数据采集网关
     * @Author: junyang.li
     * @Date: 14:41 2019/10/11
     * @param userInfo : 当前用户
     * @param rq :  新增参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> addFactoryGateways(AuthPlatformUserInfo userInfo, FactoryGatewayRQ rq) {
        //查询网关编号是否重复
        PlcFactoryGateway gateway=this.selectOne(new EntityWrapper<PlcFactoryGateway>()
                .where("deleted=0 and hc_gateway_id={0}",rq.getHcGatewayId()));
        if(gateway==null){
            //不存在则新增
            gateway=new PlcFactoryGateway(rq.getHcGatewayId(),
                    rq.getHcGatewayName(),userInfo.getFactoryId(),rq.getStatus())
            .setDeleted(Status.FALSE.getKey()).setCreateId(userInfo.getId())
            .setCreator(userInfo.getName()).setCreateTime(new Date())
            .setModifyTime(new Date());
            this.insert(gateway);
        }else {
            //修改
            gateway.setStatus(rq.getStatus()).setHcGatewayName(rq.getHcGatewayName())
            .setModifyId(userInfo.getId()).setModifier(userInfo.getName())
            .setModifyTime(new Date());
            this.updateById(gateway);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * @notes: 禁用或启用PLC数据采集网关
     * @Author: junyang.li
     * @Date: 14:53 2019/10/11
     * @param userInfo : 当前用户
     * @param hcGatewayId :  新增的网关id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    public ResponseResult<ResCodeEnum> updateFactoryGateways(AuthPlatformUserInfo userInfo, String hcGatewayId) {
        //查询网关是否是当前企业的网关
        PlcFactoryGateway gateway=this.selectOne(new EntityWrapper<PlcFactoryGateway>()
                .where("deleted=0 and factory_id={0} and hc_gateway_id={1} ",userInfo.getFactoryId(),
                        hcGatewayId));
        //判空，
        if(gateway==null){
            //不是本工厂的网关
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_OUR_GATEWAY);
        }
        //修改网关状态
        int status=Status.TRUE.getKey().equals(gateway.getStatus())?Status.FALSE.getKey():Status.TRUE.getKey();
        this.updateById(new PlcFactoryGateway()
                .setId(gateway.getId())
                .setStatus(status)
                .setModifyId(userInfo.getId())
                .setModifier(userInfo.getName())
                .setModifyTime(new Date()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * @notes: 删除网关
     * @Author: junyang.li
     * @Date: 14:53 2019/10/11
     * @param userInfo : 当前用户
     * @param hcGatewayId : 待删除的网关id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> deleteFactoryGateways(AuthPlatformUserInfo userInfo, String hcGatewayId) {
        //查询网关是否是当前企业的网关
        PlcFactoryGateway gateway=this.selectOne(new EntityWrapper<PlcFactoryGateway>()
                .where("deleted=0 and factory_id={0} and hc_gateway_id={1} ",
                        userInfo.getFactoryId(),hcGatewayId));
        //判空，
        if(gateway==null){
            //网关不存
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_OUR_GATEWAY);
        }
        this.updateById(new PlcFactoryGateway()
                .setId(gateway.getId())
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userInfo.getId())
                .setModifier(userInfo.getName())
                .setModifyTime(new Date()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes: 通过网关id查询网关数据
     * @Author: junyang.li
     * @Date: 20:40 2019/10/11
     * @param hcGatewayId : 网关id
     * @return: com.bee.platform.cloud.si.manufacture.entity.PlcFactoryGateway
     */
    @Override
    public PlcFactoryGateway getGatewayByHcGatewayId(String hcGatewayId) {
        return this.selectOne(new EntityWrapper<PlcFactoryGateway>()
                .where("status =1 and hc_gateway_id={0}",hcGatewayId));
    }

    /**
     * @notes: 从数据库中查询工厂配置的网关列表
     * @Author: junyang.li
     * @Date: 14:12 2019/10/11
     * @param param : 参数
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcFactoryGateway>
     */
    public List<PlcFactoryGateway> selectFactoryGateways(FactoryGatewayParam param){
        return plcFactoryGatewayMapper.selectFactoryGateways(param);
    }
    /**
     * @notes: 从数据库中查询工厂配置的网关列表
     * @Author: junyang.li
     * @Date: 14:12 2019/10/11
     * @param param : 参数
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcFactoryGateway>
     */
    public List<PlcFactoryGateway> selectFactoryGateways(FactoryGatewayParam param,Pagination pagination){
        return plcFactoryGatewayMapper.selectFactoryGateways(param,pagination);
    }
}
