package com.bee.platform.cloud.si.manufacture.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.FieldTypeDTO;
import com.bee.platform.cloud.si.manufacture.dto.PlcFieldConfigDTO;
import com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig;
import com.bee.platform.cloud.si.manufacture.rq.PlcFieldConfigRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;
import java.util.Set;

/**
 * plc字段相关的配置 服务类
 *
 * @author junyang.li
 * @since 2019-10-11
 */
public interface PlcFieldConfigService extends IService<PlcFieldConfig> {
    /**
     * @notes: 获取当前PLC配置的下料斗
     * @Author: junyang.li
     * @Date: 15:18 2019/10/11
     * @param plcId : plcId
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.PlcFieldConfigDTO>>
     */
    List<PlcFieldConfigDTO> getPlcFields(Integer plcId);
    /**
     * @notes: 获取当前PLC配置的下料斗
     * @Author: junyang.li
     * @Date: 15:18 2019/10/11
     * @param userInfo : 当前用户
     * @param plcId : plc Id
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.PlcFieldConfigDTO>>
     */
    ResponseResult<List<PlcFieldConfigDTO>> getPlcFields(AuthPlatformUserInfo userInfo,Integer plcId);
    /**
     * @notes: 从数据库中查询工厂配置的plc下料斗
     * @Author: junyang.li
     * @Date: 15:25 2019/10/11
     * @param factoryId : 工厂id
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig>
     */
    List<PlcFieldConfig> selectFieldsByFactory(int factoryId);
    /**
     * @notes: 从数据库中查询工厂配置的plc下料斗
     * @Author: junyang.li
     * @Date: 15:25 2019/10/11
     * @param plcId : plcId
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig>
     */
    List<PlcFieldConfig> selectFieldsByPlcId(int plcId);
    /**
     * @notes: 为当前PLC新增下料斗
     * @Author: junyang.li
     * @Date: 15:50 2019/10/11
     * @param userInfo : 当前用户
     * @param rq : 新增参数
     * @param plcId : plcId
     * @return: com.bee.platform.common.entity.ResCodeEnum
     */
    ResponseResult<ResCodeEnum> addPlcFields(AuthPlatformUserInfo userInfo,Integer plcId, List<PlcFieldConfigRQ> rq);
    /**
     * @notes: 批量修改漏斗的相关信息
     * @Author: junyang.li
     * @Date: 15:34 2019/10/21
     * @param userInfo : 当前操作人
     * @param plcId : plcId
     * @param rq : 参数
     * @return: void
     */
    ResponseResult<ResCodeEnum> updatePlcFields(AuthPlatformUserInfo userInfo,Integer plcId, List<PlcFieldConfigRQ> rq);
    /**
     * @notes: 删除当前plc的下料斗
     * @Author: junyang.li
     * @Date: 20:01 2019/10/11
     * @param userInfo : 当前用户
     * @param field : 待删除字段
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> deleteField(AuthPlatformUserInfo userInfo,String field);

    /**
     * @notes: PLC下料斗类型
     * @Author: junyang.li
     * @Date: 11:30 2019/10/21
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.FieldTypeDTO>>
     */
    ResponseResult<List<FieldTypeDTO>> getFieldTypes();
    /**
     * @notes: 通过漏斗业务id查询漏斗中类型为料批的信息
     * @Author: junyang.li
     * @Date: 13:53 2019/10/21
     * @param field :
     * @return: java.util.Map<java.lang.String,com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig>
     */
    List<String> getBatchField(Set<String> field);
}
