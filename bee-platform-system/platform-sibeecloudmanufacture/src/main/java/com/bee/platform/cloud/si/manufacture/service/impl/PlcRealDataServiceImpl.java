package com.bee.platform.cloud.si.manufacture.service.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.PlcRealDataMapper;
import com.bee.platform.cloud.si.manufacture.dto.BlankingByMqttDTO;
import com.bee.platform.cloud.si.manufacture.dto.GatewayRealDataDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProductionOutStorageDetailDTO;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.rq.ProIngredientRQ;
import com.bee.platform.cloud.si.manufacture.service.PlcFactoryGatewayService;
import com.bee.platform.cloud.si.manufacture.service.PlcFieldConfigService;
import com.bee.platform.cloud.si.manufacture.service.PlcRealDataService;
import com.bee.platform.cloud.si.manufacture.service.StorageService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigPlcDeviceService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.*;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BigDecimalUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.util.*;

/**
 * <p>
 * plc硬件通过mqtt传输的实时数据 服务实现类
 * </p>
 *
 * @author MP123
 * @since 2019-10-10
 */
@Slf4j
@Service
public class PlcRealDataServiceImpl extends ServiceImpl<PlcRealDataMapper, PlcRealData> implements PlcRealDataService {

    @Autowired
    private PlcFactoryGatewayService plcFactoryGatewayService;

    @Autowired
    private PlcFieldConfigService plcFieldConfigService;

    @Autowired
    private PlcRealDataMapper plcRealDataMapper;

    @Autowired
    private ProMaterialBatchDetailService proMaterialService;

    @Autowired
    private ProIngredientService proIngredientService;

    @Autowired
    private ProIngredientStatisticService proIngredientStatisticService;

    @Autowired
    private ProMaterialBatchService proMaterialBatchService;

    @Autowired
    private StorageService storageService;

    @Autowired
    private ConfigPlcDeviceService configPlcDeviceService;

    @Autowired
    private ProIngredientRecordService proIngredientRecordService;
    /**
     * 产品单位判断，暂时写死
     */
    private static final String TON="吨";
    /**
     * 在内存中存入每一次入库的实时数据，避免每次都从数据库中校验数据是否重复
     */
    private static final Map<String,String> CURRENT_MAP=new HashMap<>(16);
    /**
     * @notes: 处理plc采集的数据
     * @Author: junyang.li
     * @Date: 18:52 2019/10/10
     * @param dto : 采集数据
     * @return: void
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void tacklePlcData(GatewayRealDataDTO dto) {
        //实时数据
        Map<String,String> realMap=dto.getMap();
        boolean result=this.checkRepeatData(realMap);
        if(!result){
            //log.info("重复数据:{}",JSON.toJSONString(dto));
            return;
        }
        //根据网关id查询网关是否存在
        PlcFactoryGateway gateway=plcFactoryGatewayService.getGatewayByHcGatewayId(dto.getHcGatewayId());
        if(gateway==null){
            //log.info("网关{}不存在，或网关已被禁用。",dto.getHcGatewayId());
            return;
        }
        //查询该工程所有的PLC设备和PLC漏斗详细
        List<PlcFieldConfig> list=plcFieldConfigService.selectFieldsByFactory(gateway.getFactoryId());
        //判空
        if(CollectionUtils.isEmpty(list)){
            log.info("工厂：id={}未添加任何的PLC漏斗信息，无法储存MQTT中的数据:{}",dto);
            return;
        }
        List<PlcRealData> all=new ArrayList<>();
        //实时数据容器 键是PLC id
        Map<Integer,List<PlcRealData>> map=new HashMap<>(16);
        list.forEach(obj->{
            //漏斗id
            String funnel=obj.getField();
            //MQTT 中漏斗的值
            String value=realMap.get(funnel);
            if(value!=null){
               //PLC ID
               Integer plcId=obj.getPlcId();
               //容器
               List<PlcRealData> item=map.get(obj.getPlcId());
               //实时数据对象
               PlcRealData plcRealData=new PlcRealData(funnel,value,plcId,
                       dto.getReceiveTime(),Status.TRUE.getKey());
                all.add(plcRealData);
               if(CollectionUtils.isEmpty(item)){
                   item=new ArrayList<>();
                   item.add(plcRealData);
                   map.put(plcId,item);
               }else {
                   item.add(plcRealData);
               }
            }
        });
        //批量插入
        plcRealDataMapper.insertAll(all);
        //将MQTT中的数据实时更新至下料
        this.updateBlankingByMqttData(map);
        this.isPreserveData(map);
        //更新内存中的最新数据
        CURRENT_MAP.putAll(realMap);
        log.info("实时数据:{}", JSON.toJSONString(dto));
    }

    /**
     * @notes: 当MQTT接收到实时数据时，通过料批字段的值来判断是否是重复数据
     * @Author: junyang.li
     * @Date: 14:07 2019/10/18
     * @param realMap :
     * @return: boolean
     */
    private boolean checkRepeatData(Map<String,String> realMap){
        //通过漏斗业务id查询漏斗中类型为料批的信息
        List<String> batch=plcFieldConfigService.getBatchField(realMap.keySet());
        if(CollectionUtils.isEmpty(batch)){
            log.error("下料斗不存在,获得的下料斗数据是:{}",JSON.toJSONString(realMap));
        }
        //默认不相同
        boolean isChange=false;
        //遍历
        for (String item:batch) {
            String now=realMap.get(item);
            String old=CURRENT_MAP.get(item);
            //不相同直接返回true
            if(!now.equals(old)){
                isChange=true;
                break;
            }
        }
        //再从数据库从校验是否是重复
        if(isChange){
            //再从数据库从校验是否是重复
            Set<String> set=realMap.keySet();
            List<PlcRealData> list=plcRealDataMapper.getNewData(set,set.size());
            //没有最新数据 或漏斗数量发生变化
            if(CollectionUtils.isEmpty(list) || list.size()!=set.size()){
                return true;
            }
            //遍历查询数据是否发生变化
            for (PlcRealData item:list) {
                String key=item.getFiled();
                String value=item.getValue();
                String old=realMap.get(key);
                if(!value.equals(old)){
                    return true;
                }
            }
        }
        return false;
    }
    /**
     * @notes: 判断是否需要每隔八小时自动保存一次配料记录
     * @Author: junyang.li
     * @Date: 15:48 2019/10/12
     * @return: void
     */
    private void isPreserveData(Map<Integer,List<PlcRealData>> map){
        for (Map.Entry<Integer,List<PlcRealData>> item:map.entrySet()) {
            Integer plcId=item.getKey();
            List<PlcRealData> list=item.getValue();
            Map<String,String> realMap=new HashMap<>(16);
            list.forEach(obj->realMap.put(obj.getFiled(),obj.getValue()));
            //通过漏斗业务id查询漏斗中类型为料批的信息
            List<String> batch=plcFieldConfigService.getBatchField(realMap.keySet());
            //默认不需要保存
            boolean isPreserveData=false;
            //根据料批归0 来判断是否保存，所有的料批都归0就自动保存
            for (String str:batch) {
                String value=realMap.get(str);
                isPreserveData="0".equals(value);
            }
            //所有的料批都归0就自动保存
            if(isPreserveData){
                ConfigPlcDevice plcDevice=configPlcDeviceService.selectById(plcId);
                if(plcDevice!=null){
                    AuthPlatformUserInfo userInfo=new AuthPlatformUserInfo().setId(0)
                            .setOrgId(plcDevice.getEnterpriseId())
                            .setFactoryId(plcDevice.getFactoryId());
                    //自动保存
                    proIngredientService.addIngredient(new ProIngredientRQ()
                            .setPlcId(plcId)
                            .setTimeSave(Status.TRUE.getKey()),userInfo);
                }
            }
        }
    }
    /**
     * @notes: 配料统计
     * @Author: junyang.li
     * @Date: 16:02 2019/10/12
     * @param plcId : 
     * @return: java.util.Map<java.lang.Integer,java.math.BigDecimal> 键是产品id  值是产品数量
     */
    @Override
    public Map<Integer, BigDecimal> getPlcCountData(int plcId){
        //配料最近一次保存时间
        Date lastPreserveDate=proIngredientService.getLastPreserve(plcId);
        if(lastPreserveDate==null){
            ProMaterialBatch batch=proMaterialBatchService.selectOne(new EntityWrapper<ProMaterialBatch>()
                    .where("status=1 and plc_id={0}",plcId)
                    .orderBy("create_time",false).last("limit 1"));
            lastPreserveDate=batch.getCreateTime();
        }
        //从实时数据中查询
        List<PlcRealData> list=this.selectList(new EntityWrapper<PlcRealData>()
                .where("status=1 and plc_id={0}",plcId)
                .and()
                .gt("time",lastPreserveDate));
        //判空
        if(CollectionUtils.isEmpty(list)){
            return new HashMap<>(1);
        }
        //键是漏斗业务id ，值是所有数据之和
        Map<String,BigDecimal> map=new HashMap<>(16);
        list.forEach(obj->{
            BigDecimal value=map.get(obj.getFiled());
            value=BigDecimalUtils.add(value,BigDecimalUtils.strToDecimal(obj.getValue()));
            map.put(obj.getFiled(),value);
        });
        //通过漏斗业务id查询对应的产品id
        List<ProMaterialBatchDetail> details=proMaterialService.selectList(new EntityWrapper<ProMaterialBatchDetail>()
                .where("status =1").and().in("plc_field",map.keySet()));
        if(CollectionUtils.isEmpty(details)){
            return new HashMap<>(1);
        }
        //键是产品id  值是产品数量
        Map<Integer, BigDecimal> product=new HashMap<>(16);
        details.forEach(obj->{
            Integer productId=obj.getProductId();
            String unit=obj.getUnit();
            BigDecimal number=BigDecimalUtils.isNull(map.get(obj.getPlcField()));
            if(TON.equals(unit)){
                number=BigDecimalUtils.divide(number,BigDecimalUtils.THOUSAND,
                        BigDecimalUtils.THREE_SCALE);
            }
            product.put(productId,number);
        });
        return product;
    }

    /**
     * @notes: 将MQTT 中的数据实时更新到下料数据中
     * @Author: junyang.li
     * @Date: 16:46 2019/10/12
     * @param map : 待更新数据 键是 plc_id  值是对应的实时数据
     * @return: void
     */
    private void updateBlankingByMqttData(Map<Integer,List<PlcRealData>> map){
        //判空
        if(CollectionUtils.isEmpty(map)){
            return;
        }
        //非空，则遍历
        map.forEach((key,value)->{
            //遍历实时数据
            Map<String,BigDecimal> data=new HashMap<>(16);
            //遍历并转换为map对象，方便获取值
            value.forEach(obj->data.put(obj.getFiled(),BigDecimalUtils.strToDecimal(obj.getValue())));
            ProMaterialBatch proMaterialBatch=proMaterialBatchService.selectOne(new EntityWrapper<ProMaterialBatch>()
                    .where("status =1 and active=1 and plc_id={0}",key));
            if(proMaterialBatch==null){
                log.error("通过PlcId={}，无法查询到料批的相关信息.无法新增数据",key);
                return ;
            }
            //通过漏斗业务id查询对应的产品id
            List<ProMaterialBatchDetail> details=proMaterialService.selectList(new EntityWrapper<ProMaterialBatchDetail>()
                    .where("status =1 and batch_id={0}",proMaterialBatch.getId()));
            if(CollectionUtils.isEmpty(details)){
                log.error("漏斗业务id未关联产品id，无法继续更新实时下料数据。");
                return ;
            }
            //遍历获得产品id对的下料数据
            List<BlankingByMqttDTO> list=new ArrayList<>();
            //产品id对应的plc实时数据
            Map<String,BigDecimal> realData=new HashMap<>(16);
            //产品id 对应MQTT获取的实时下料数量
            details.forEach(obj->{
                Integer productId=obj.getProductId();
                BigDecimal number=BigDecimalUtils.isNull(data.get(obj.getPlcField()));
                if(TON.equals(obj.getUnit())){
                    number=BigDecimalUtils.divide(number,BigDecimalUtils.THOUSAND,
                            BigDecimalUtils.THREE_SCALE);
                }
                list.add(new BlankingByMqttDTO(productId, obj.getProductSpecId(), number));
                realData.put(productId + "-" + obj.getProductSpecId(),number);
            });
            //下料处实时增加数据
            proIngredientStatisticService.updateBlankingByMqttData(proMaterialBatch.getId(),list);
            //配料处实时新增数据
            proIngredientRecordService.updateBlankingByMqttData(proMaterialBatch.getId(),list);

            //this.deductStock(key,realData);
        });
    }
    /**
     * @notes: 根据实时数据扣减库存 不需要在我这边扣库存
     * @Author: junyang.li
     * @Date: 16:10 2019/10/28
     * @param plcId :
     * @param realData :
     * @return: void
     */
    private void deductStock(Integer plcId,Map<String,BigDecimal> realData){
        //实时扣减库存信息
        ProMaterialBatch batch=proMaterialBatchService.selectOne(new EntityWrapper<ProMaterialBatch>()
                .where("active=1 and plc_id={0}",plcId));
        //PLC 对应的料批
        if(batch!=null){
            //通过料批id查询出产品对应的仓库id
            List<ProIngredientStatistic> statistics=proIngredientStatisticService.selectList(new EntityWrapper<ProIngredientStatistic>()
                    .where("status=1 and batch_id={0}",batch.getId()));
            //判空
            if(!CollectionUtils.isEmpty(statistics)){
                statistics.forEach(obj->{
                    ProductionOutStorageDetailDTO dto= new ProductionOutStorageDetailDTO()
                            .setProductId(obj.getProductId())
                            .setProductName(obj.getProductName())
                            .setProductNumber(BigDecimalUtils.isNull(realData.get(obj.getProductId() + "-" + obj.getProductSpecId())))
                            .setProductSpecId(obj.getProductSpecId())
                            .setProductSpecName(obj.getProductSpecName())
                            .setProductUnit(obj.getUnit())
                            .setStorageId(obj.getWarehouseId())
                            .setStorageName(obj.getWarehouseName());
                    storageService.saveProductionOutStorage(dto,new AuthPlatformUserInfo());
                });
            }
        }
    }
}
