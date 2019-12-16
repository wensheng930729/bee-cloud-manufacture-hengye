package com.bee.platform.cloud.si.manufacture.service.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.PlcFieldConfigMapper;
import com.bee.platform.cloud.si.manufacture.dto.FieldTypeDTO;
import com.bee.platform.cloud.si.manufacture.dto.PlcFieldConfigDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigPlcDevice;
import com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig;
import com.bee.platform.cloud.si.manufacture.rq.PlcFieldConfigRQ;
import com.bee.platform.cloud.si.manufacture.service.PlcFieldConfigService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigPlcDeviceService;
import com.bee.platform.common.constants.enums.FieldType;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import redis.clients.jedis.exceptions.JedisException;

import java.util.*;
import java.util.stream.Collectors;

/**
 * plc字段相关的配置 服务实现类
 *
 * @author junyang.li
 * @since 2019-10-11
 */
@Slf4j
@Service
public class PlcFieldConfigServiceImpl extends ServiceImpl<PlcFieldConfigMapper, PlcFieldConfig> implements PlcFieldConfigService {

    @Autowired
    private PlcFieldConfigMapper plcFieldConfigMapper;

    @Autowired
    private ConfigPlcDeviceService configPlcDeviceService;

    @Autowired
    private JedisService jedisService;
    /**
     * @notes: 获取当前PLC配置的下料斗
     * @Author: junyang.li
     * @Date: 15:18 2019/10/11
     * @param plcId : plc Id
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.PlcFieldConfigDTO>>
     */
    @Override
    public List<PlcFieldConfigDTO> getPlcFields(Integer plcId) {
        List<PlcFieldConfig> list=this.selectFieldsByPlcId(plcId);
        //判空
        if(CollectionUtils.isEmpty(list)){
            return new ArrayList<>();
        }
        //遍历
        return list.stream().map(obj->{
            return new PlcFieldConfigDTO(obj.getField(),obj.getFieldName(),obj.getPlcId(),
                    obj.getFieldType(),FieldType.getValueByKey(obj.getFieldType()));
        }).collect(Collectors.toList());
    }
    /**
     * @notes: 获取当前PLC配置的下料斗
     * @Author: junyang.li
     * @Date: 15:18 2019/10/11
     * @param userInfo : 当前用户
     * @param plcId : plc Id
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.PlcFieldConfigDTO>>
     */
    @Override
    public ResponseResult<List<PlcFieldConfigDTO>> getPlcFields(AuthPlatformUserInfo userInfo,Integer plcId) {
        int factoryId=userInfo.getFactoryId();
        //判断plc是否是当前工厂的plc
        ConfigPlcDevice configPlcDevice=configPlcDeviceService.getPlcByFactoryId(factoryId,plcId);
        if(configPlcDevice==null){
            log.info("当前用户无法通过工厂id={}查询出plcId={}的详细信息",factoryId,plcId);
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
        }
        List<PlcFieldConfig> list=this.selectFieldsByPlcId(plcId);
        //判空
        if(CollectionUtils.isEmpty(list)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new ArrayList<>());
        }
        //遍历
        List<PlcFieldConfigDTO> items=list.stream().map(obj->{
            return BeanUtils.copyProperties(obj,PlcFieldConfigDTO.class)
                    .setPlcId(plcId);
        }).collect(Collectors.toList());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,items);
    }
    /**
     * @notes: 从数据库中查询工厂配置的plc下料斗
     * @Author: junyang.li
     * @Date: 15:25 2019/10/11
     * @param factoryId : 工厂id
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig>
     */
    @Override
    public List<PlcFieldConfig> selectFieldsByFactory(int factoryId){
        return plcFieldConfigMapper.selectFieldsByFactory(factoryId);
    }
    /**
     * @notes: 从数据库中查询工厂配置的plc下料斗
     * @Author: junyang.li
     * @Date: 15:25 2019/10/11
     * @param plcId : plcId
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig>
     */
    @Override
    public List<PlcFieldConfig> selectFieldsByPlcId(int plcId) {
        return this.selectList(new EntityWrapper<PlcFieldConfig>().where("deleted=0 and plc_id={0}",plcId));
    }
    /**
     * @notes: 为当前PLC新增下料斗
     * @Author: junyang.li
     * @Date: 15:50 2019/10/11
     * @param userInfo : 当前用户
     * @param list : 新增参数
     * @param plcId : plcId
     * @return: com.bee.platform.common.entity.ResCodeEnum
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> addPlcFields(AuthPlatformUserInfo userInfo, Integer plcId, List<PlcFieldConfigRQ> list) {
        int factoryId=userInfo.getFactoryId();
        Map<String,PlcFieldConfigRQ> map=new HashMap<>(16);
        //遍历成map对象，方便后面获取数据
        list.forEach(obj->map.put(obj.getField(),obj));
        //拿到所有字段的英文名称
        Set<String> set=map.keySet();
        //从数据库中查询是否已存在，并返回已存在的字段
        Set<String> exist=plcFieldConfigMapper.selectNotExistField(plcId,set);
        //从所有的字段中移除已经存在的字段
        List<String> notExist=new ArrayList<>();
        //遍历所有字段
        for (String item:set) {
            if(!exist.contains(item)){
                notExist.add(item);
            }
        }
        //判空
        if(CollectionUtils.isEmpty(notExist)){
            log.info("所有字段均已存在，无需重复添加,当前工厂id是：{},传入参数是:{}",factoryId,list);
            return ResponseResult.buildResponseResult(ResCodeEnum.FIELD_IS_EXIST);
        }
        //为当前工厂新增不存在的字段
        List<PlcFieldConfig> configs=notExist.stream().map(obj->{
           return BeanUtils.copyProperties(map.get(obj),PlcFieldConfig.class)
                   .setFactoryId(factoryId)
                   .setPlcId(plcId)
                   .setCreateId(userInfo.getId())
                   .setCreator(userInfo.getName())
                   .setCreateTime(new Date())
                   .setModifyTime(new Date())
                   .setStatus(Status.TRUE.getKey())
                   .setDeleted(Status.FALSE.getKey());
        }).collect(Collectors.toList());
        //批量插入
        plcFieldConfigMapper.insertAll(configs);
        //清除缓存
        jedisService.delKey(ConstantsUtil.ALL_FIELD_CONFIG_KEY);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * @notes: 批量修改漏斗的相关信息
     * @Author: junyang.li
     * @Date: 15:34 2019/10/21
     * @param userInfo : 当前操作人
     * @param plcId : plcId
     * @param rq : 参数
     * @return: void
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updatePlcFields(AuthPlatformUserInfo userInfo, Integer plcId, List<PlcFieldConfigRQ> rq) {
        //删除该plc对应的漏斗数据
        this.delete(new EntityWrapper<PlcFieldConfig>().where("plc_id={0}",plcId));
        //然后重新新增
        return this.addPlcFields(userInfo,plcId,rq);
    }

    /**
     * @notes: 删除当前plc的下料斗
     * @Author: junyang.li
     * @Date: 20:01 2019/10/11
     * @param userInfo : 当前用户
     * @param field : 待删除字段
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> deleteField(AuthPlatformUserInfo userInfo, String field) {
        //查询字段是否存在
        PlcFieldConfig config=this.selectOne(new EntityWrapper<PlcFieldConfig>()
                .where("deleted=0 and field={0}",field));
        if(config==null){
            log.info("plc下料斗不存在。下料斗id={}",field);
            return ResponseResult.buildResponseResult(ResCodeEnum.PLC_FIELD_NOT_FOUND);
        }
        int factoryId=userInfo.getFactoryId();
        int plcId=config.getPlcId();
        //查询plcId是否存在
        ConfigPlcDevice configPlcDevice=configPlcDeviceService.getPlcByFactoryId(factoryId,plcId);
        if(configPlcDevice==null){
            log.info("当前用户无法通过工厂id={}查询出plcId={}的详细信息",factoryId,plcId);
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
        }
        //删除该下料斗
        this.updateById(new PlcFieldConfig().setId(config.getId()).setDeleted(Status.FALSE.getKey())
            .setModifyId(userInfo.getId()).setModifier(userInfo.getName()).setModifyTime(new Date()));
        //清除缓存
        jedisService.delKey(ConstantsUtil.ALL_FIELD_CONFIG_KEY);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * @notes: PLC下料斗类型
     * @Author: junyang.li
     * @Date: 11:30 2019/10/21
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.FieldTypeDTO>>
     */
    @Override
    public ResponseResult<List<FieldTypeDTO>> getFieldTypes() {
        FieldType[] types=FieldType.values();
        List<FieldTypeDTO> list=new ArrayList<>();
        for (FieldType item:types) {
            list.add(new FieldTypeDTO(item.getKey(),item.getValue()));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
    }

    /**
     * @notes: 通过漏斗业务id查询漏斗中类型为料批的信息
     * @Author: junyang.li
     * @Date: 13:53 2019/10/21
     * @param field :
     * @return: java.util.Map<java.lang.String,com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig>
     */
    @Override
    public List<String> getBatchField(Set<String> field){
        if(CollectionUtils.isEmpty(field)){
            return new ArrayList<>();
        }
        //获得所有的字段信息
        List<PlcFieldConfig> all=this.getAllFieldsFormRedis();
        return all.stream().map(obj->{
            boolean result=field.contains(obj.getField());
            //如果存在，并且类型为料批，则返回
            if(result && FieldType.BATCH.getKey().equals(obj.getFieldType())){
                return obj.getField();
            }
            return null;
        }).filter(Objects::nonNull).collect(Collectors.toList());
    }

    /**
     * @notes: 从缓存中获得漏斗的相关信息
     * @Author: junyang.li
     * @Date: 13:31 2019/10/21
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig>
     */
    private List<PlcFieldConfig> getAllFieldsFormRedis(){
        String key= ConstantsUtil.ALL_FIELD_CONFIG_KEY;
        //从缓存中获取所有的漏斗配置
       try {
           String value=jedisService.get(key);
           //判空
           if(value==null){
               //从数据中查询
               List<PlcFieldConfig> list=this.getFieldFromDB();
               jedisService.set(key, JSON.toJSONString(list),ConstantsUtil.OVERDUE);
               return list;
           }
           return JSON.parseArray(value,PlcFieldConfig.class);
       }catch (JedisException e){
           log.error("从缓存中查询漏斗的信息失败，返回的异常信息是:{}",e);
           //从数据中查询
           return this.getFieldFromDB();
       }
    }

    /**
     * @notes: 从数据库中查询所有的漏斗数据
     * @Author: junyang.li
     * @Date: 13:22 2019/10/21
     * @return: java.util.Map<java.lang.Integer,java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig>>
     */
    private  List<PlcFieldConfig> getFieldFromDB(){
        return plcFieldConfigMapper.selectAll();
    }

}
