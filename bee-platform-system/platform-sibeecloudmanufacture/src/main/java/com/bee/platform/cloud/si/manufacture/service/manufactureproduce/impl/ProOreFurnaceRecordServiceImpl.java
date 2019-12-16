package com.bee.platform.cloud.si.manufacture.service.manufactureproduce.impl;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProOreFurnaceRecordDetailMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProOreFurnaceRecordMapper;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.ProOreFurnaceRecord;
import com.bee.platform.cloud.si.manufacture.entity.ProOreFurnaceRecordDetail;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProOreFurnaceRecordService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * 矿热炉记录 服务实现类
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-26
 */
@Slf4j
@Service
public class ProOreFurnaceRecordServiceImpl extends ServiceImpl<ProOreFurnaceRecordMapper, ProOreFurnaceRecord> implements ProOreFurnaceRecordService {

    @Autowired
    private ProOreFurnaceRecordDetailMapper oreFurnaceRecordDetailMapper;
    @Autowired
    private ProOreFurnaceRecordMapper oreFurnaceRecordMapper;
    @Autowired
    private UserInfoUtils userInfoUtils;

    /**
     * @param rq
     * @param userInfo
     * @return
     * @descriptin 新增矿热炉记录基本信息
     * @author xin.huang
     * @date 2019/9/26
     */
    @Override
    public ResponseResult addOreRecord(ProOreFurnaceRecordRQ rq, AuthPlatformUserInfo userInfo) {
        ProOreFurnaceRecord proOreFurnaceRecord = BeanUtils.copyProperties(rq, ProOreFurnaceRecord.class);
        proOreFurnaceRecord.setCreateId(userInfo.getId())
                .setCompanyId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setCreateTime(new Date())
                .setOnduty(Status.TRUE.getKey());
        if (!this.insert(proOreFurnaceRecord)) {
            log.error("添加矿热炉记录信息失败 类：{} 方法：{}", "ProOreFurnaceRecordServiceImpl", "addOreRecord");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                BeanUtils.copyProperties(proOreFurnaceRecord, ProOreFurnaceRecordDTO.class));
    }

    /**
     * @param rq
     * @param userInfo
     * @return
     * @descriptin 更新矿热炉记录基本信息
     * @author xin.huang
     * @date 2019/9/26
     */
    @Override
    public ResponseResult updateOreRecord(ProOreFurnaceRecordRQ rq, AuthPlatformUserInfo userInfo) {
        ProOreFurnaceRecord record = BeanUtils.copyProperties(rq, ProOreFurnaceRecord.class);
        record.setModifyId(userInfo.getId()).setModifyTime(new Date());
        this.updateById(record);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @param rq
     * @param userInfo
     * @return
     * @descriptin 查询矿热炉当前是否有记录员
     * @author xin.huang
     * @date 2019/9/26
     */
    @Override
    public ResponseResult<ProOreRecordUserDTO> getOreRecordOnduty(ProOreFurnaceRecordQueryRQ rq, AuthPlatformUserInfo userInfo) {
        Wrapper<ProOreFurnaceRecord> wrapper = new EntityWrapper<ProOreFurnaceRecord>().eq("status", Status.TRUE.getKey());
        if (!ObjectUtils.isEmpty(rq.getFurnaceId())) {
            wrapper.eq("furnace_id", rq.getFurnaceId());
        }
//        if (!ObjectUtils.isEmpty(rq.getShift())) {
////            wrapper.eq("shift", rq.getShift());
////        }
        if (!ObjectUtils.isEmpty(rq.getOpenTime())) {
            wrapper.addFilter("DATE_FORMAT(open_time,'%Y-%m-%d')={0}", rq.getOpenTime());
        }
        wrapper.eq("company_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("onduty", Status.TRUE.getKey());
        List<ProOreFurnaceRecord> records = this.selectList(wrapper);
        //当前没有任何记录员
        Integer ondutyFlag = 0;
        ProOreRecordUserDTO recordUser = new ProOreRecordUserDTO();
        Integer userId = null;
        if (!CollectionUtils.isEmpty(records)) {
            for (ProOreFurnaceRecord record : records) {
                //当前操作人已记录当前炉号数据，并未交班
                if (userInfo.getId().equals(record.getCreateId())) {
                    userId = record.getCreateId();
                    recordUser.setOreRecordId(record.getId());
                    ondutyFlag = 2;
                    break;
                } else {
                    //当前炉号存在其他人员正在记录数据
                    userId = record.getCreateId();
                    ondutyFlag = 1;
                    break;
                }
            }
        }
        if (!ObjectUtils.isEmpty(userId)) {
            AuthPlatformUserInfo user = userInfoUtils.getUserById(userInfo.getSysToken(), userId);
            if (!ObjectUtils.isEmpty(user)) {
                recordUser.setName(user.getName()).setPhone(user.getUsername());
            }
        }
        recordUser.setOnduty(ondutyFlag);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, recordUser);
    }

    /**
     * @param userInfo
     * @return
     * @descriptin 新增矿热炉记录明细信息
     * @author xin.huang
     * @date 2019/9/26
     */
    @Override
    public ResponseResult addOreRecordDetail(ProOreFurnaceRecordDetailRQ rq, AuthPlatformUserInfo userInfo) {
        ProOreFurnaceRecordDetail detail = BeanUtils.copyProperties(rq, ProOreFurnaceRecordDetail.class);
        detail.setCreateTime(new Date());
        if (!ObjectUtils.isEmpty(rq.getStirFurnaceRecordList())) {
            detail.setStirFurnaceRecord(JSONObject.toJSONString(rq.getStirFurnaceRecordList()));
        }
        //校验电极交时长度是否为空
        List<ProElectrodeRQ> electrodeList = rq.getElectrodeList();
        boolean endLengthNotExist = false;
        if (ObjectUtils.isEmpty(electrodeList)) {
            endLengthNotExist = true;
        }
        for (ProElectrodeRQ electrodeRQ : electrodeList) {
            if (ObjectUtils.isEmpty(electrodeRQ.getEndLength())) {
                endLengthNotExist = true;
                break;
            }
        }
        if (endLengthNotExist) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ORE_FURNACE_ELECTRODE_ENDLENGTH_NOT_NULL);
        }
        detail.setElectrode(JSONObject.toJSONString(electrodeList));

        if (!ObjectUtils.isEmpty(rq.getOutFurnaceRecordRQ())) {
            detail.setOutFurnaceRecord(JSONObject.toJSONString(rq.getOutFurnaceRecordRQ()));
        }
        //校验交时动力电是否为空
        ProPowerRecordRQ powerRecordRQ = rq.getPowerRecordRQ();
        if (ObjectUtils.isEmpty(powerRecordRQ) || StringUtils.isBlank(powerRecordRQ.getHandover())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ORE_FURNACE_POWERRECORD_HANDOVER_NOT_NULL);
        }
        if (StringUtils.isNotBlank(powerRecordRQ.getConsume())) {
            detail.setPowerConsume(new BigDecimal(powerRecordRQ.getConsume()));
        }
        detail.setPowerRecord(JSONObject.toJSONString(powerRecordRQ));

        int flag = oreFurnaceRecordDetailMapper.insert(detail);
        if (flag <= 0) {
            log.error("添加矿热炉记录明细信息失败 类：{} 方法：{}", "ProOreFurnaceRecordServiceImpl", "addOreRecordDetail");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @param userInfo
     * @return
     * @descriptin 更新矿热炉记录明细信息
     * @author xin.huang
     * @date 2019/9/26
     */
    @Override
    public ResponseResult updateOreRecordDetail(ProOreFurnaceRecordDetailRQ rq, AuthPlatformUserInfo userInfo) {
        oreFurnaceRecordDetailMapper.update(new ProOreFurnaceRecordDetail().setStatus(Status.FALSE.getKey()),
                new EntityWrapper<>(new ProOreFurnaceRecordDetail().setOreRecordId(rq.getOreRecordId())));
        ProOreFurnaceRecordDetail detail = BeanUtils.copyProperties(rq, ProOreFurnaceRecordDetail.class);
        detail.setCreateTime(new Date());
        if (!ObjectUtils.isEmpty(rq.getStirFurnaceRecordList())) {
            detail.setStirFurnaceRecord(JSONObject.toJSONString(rq.getStirFurnaceRecordList()));
        }

        //校验电极交时长度是否为空
        List<ProElectrodeRQ> electrodeList = rq.getElectrodeList();
        boolean endLengthNotExist = false;
        if (ObjectUtils.isEmpty(electrodeList)) {
            endLengthNotExist = true;
        }
        for (ProElectrodeRQ electrodeRQ : electrodeList) {
            if (ObjectUtils.isEmpty(electrodeRQ.getEndLength())) {
                endLengthNotExist = true;
                break;
            }
        }
//        if (endLengthNotExist) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.ORE_FURNACE_ELECTRODE_ENDLENGTH_NOT_NULL);
//        }
        detail.setElectrode(JSONObject.toJSONString(electrodeList));

        if (!ObjectUtils.isEmpty(rq.getOutFurnaceRecordRQ())) {
            detail.setOutFurnaceRecord(JSONObject.toJSONString(rq.getOutFurnaceRecordRQ()));
        }

        //校验交时动力电是否为空
        ProPowerRecordRQ powerRecordRQ = rq.getPowerRecordRQ();
        if (ObjectUtils.isEmpty(powerRecordRQ) ) { //|| StringUtils.isBlank(powerRecordRQ.getHandover())
            return ResponseResult.buildResponseResult(ResCodeEnum.ORE_FURNACE_POWERRECORD_HANDOVER_NOT_NULL);
        }

        if (StringUtils.isNotBlank(powerRecordRQ.getConsume())) {
            detail.setPowerConsume(new BigDecimal(powerRecordRQ.getConsume()));
        }
        detail.setPowerRecord(JSONObject.toJSONString(powerRecordRQ));

        int flag = oreFurnaceRecordDetailMapper.insert(detail);
        if (flag <= 0) {
            log.error("添加矿热炉记录明细信息失败 类：{} 方法：{}", "ProOreFurnaceRecordServiceImpl", "addOreRecordDetail");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @param recordId
     * @param userInfo
     * @return
     * @descriptin 查询矿热炉记录明细
     * @author xin.huang
     * @date 2019/9/26
     */
    @Override
    public ResponseResult<List<ProOreFurnaceRecordDetailDTO>> findOreRecordDetailList(Long recordId, AuthPlatformUserInfo userInfo) {
        List<ProOreFurnaceRecordDetail> details = oreFurnaceRecordDetailMapper.selectList(new EntityWrapper<>(new ProOreFurnaceRecordDetail()
                .setOreRecordId(recordId).setStatus(Status.TRUE.getKey())));
        List<ProOreFurnaceRecordDetailDTO> detailDTOS = new ArrayList<>(details.size());
        if (!CollectionUtils.isEmpty(details)) {
            details.forEach(detail -> {
                ProOreFurnaceRecordDetailDTO detailDTO = new ProOreFurnaceRecordDetailDTO();
                if (StringUtils.isNotBlank(detail.getStirFurnaceRecord())) {
                    detailDTO.setStirFurnaceRecordList(JSONArray.parseArray(detail.getStirFurnaceRecord(), ProStirFurnaceRecordDTO.class));
                }
                if (StringUtils.isNotBlank(detail.getElectrode())) {
                    detailDTO.setElectrodeList(JSONArray.parseArray(detail.getElectrode(), ProElectrodeDTO.class));
                }
                if (StringUtils.isNotBlank(detail.getOutFurnaceRecord())) {
                    detailDTO.setOutFurnaceRecordDTO(JSONObject.parseObject(detail.getOutFurnaceRecord(), ProOutFurnaceRecordDTO.class));
                }
                if (StringUtils.isNotBlank(detail.getPowerRecord())) {
                    detailDTO.setPowerRecordDTO(JSONObject.parseObject(detail.getPowerRecord(), ProPowerRecordDTO.class));
                }
                detailDTOS.add(detailDTO);
            });
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, detailDTOS);
    }

    /**
     * @param rq
     * @param userInfo
     * @return
     * @descriptin 根据炉号id和班次查询矿热炉记录明细信息
     * @author xin.huang
     * @date 2019/9/26
     */
    @Override
    public ResponseResult<ProOreFurnaceRecordDetailDTO>  getOreRecordDetail(ProOreFurnaceRecordQueryRQ rq, AuthPlatformUserInfo userInfo) {
        ProOreFurnaceRecordDetailDTO recordDetailDTO = new ProOreFurnaceRecordDetailDTO();
        rq.setCompanyId(userInfo.getOrgId()).setFactoryId(userInfo.getFactoryId());
        recordDetailDTO.setOreRecordId(rq.getOreRecordId());
        List<ProOreFurnaceRecordDetail> details = oreFurnaceRecordDetailMapper
                .selectList(new EntityWrapper<>(new ProOreFurnaceRecordDetail()
                        .setOreRecordId(rq.getOreRecordId())
                        .setStatus(Status.TRUE.getKey()))
                        .orderBy("create_time", false));
        if (!CollectionUtils.isEmpty(details)) {
            ProOreFurnaceRecordDetail detail = details.get(0);
            if (StringUtils.isNotBlank(detail.getElectrode())) {
                recordDetailDTO.setElectrodeList(JSONArray.parseArray(detail.getElectrode(), ProElectrodeDTO.class));
            }
            if (StringUtils.isNotBlank(detail.getPowerRecord())) {
                recordDetailDTO.setPowerRecordDTO(JSONArray.parseObject(detail.getPowerRecord(), ProPowerRecordDTO.class));
            }
            if (StringUtils.isNotBlank(detail.getStirFurnaceRecord())) {
                recordDetailDTO.setStirFurnaceRecordList(JSONArray.parseArray(detail.getStirFurnaceRecord(), ProStirFurnaceRecordDTO.class));
            }
            if (StringUtils.isNotBlank(detail.getOutFurnaceRecord())) {
                recordDetailDTO.setOutFurnaceRecordDTO(JSONObject.parseObject(detail.getOutFurnaceRecord(), ProOutFurnaceRecordDTO.class));
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, recordDetailDTO);
    }

    /**
     * @param furnaceId
     * @param userInfo
     * @return
     * @descriptin 根据炉号id查询上次交班时的矿热炉记录明细信息
     * @author xin.huang
     * @date 2019/9/26
     */
    @Override
    public ResponseResult<ProOreFurnaceRecordDetailDTO> getOreRecordHandOverDetail(Integer furnaceId, AuthPlatformUserInfo userInfo) {
        ProOreFurnaceRecordDetailDTO result = new ProOreFurnaceRecordDetailDTO();
        ProOreFurnaceRecordQueryRQ rq = new ProOreFurnaceRecordQueryRQ()
                .setFurnaceId(furnaceId)
                .setCompanyId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setOnduty(Status.FALSE.getKey());
        List<ProOreFurnaceRecord> records = oreFurnaceRecordMapper.findRecordsByCondition(rq);
        if (!CollectionUtils.isEmpty(records)) {
            ProOreFurnaceRecord record = records.get(0);
            List<ProOreFurnaceRecordDetail> details = oreFurnaceRecordDetailMapper
                    .selectList(new EntityWrapper<>(new ProOreFurnaceRecordDetail()
                            .setOreRecordId(record.getId())
                            .setStatus(Status.TRUE.getKey()))
                            .orderBy("create_time", false));
            if (!CollectionUtils.isEmpty(details)) {
                ProOreFurnaceRecordDetail detail = details.get(0);
                if (StringUtils.isNotBlank(detail.getElectrode())) {
                    result.setElectrodeList(JSONArray.parseArray(detail.getElectrode(), ProElectrodeDTO.class));
                }
                if (StringUtils.isNotBlank(detail.getPowerRecord())) {
                    result.setPowerRecordDTO(JSONArray.parseObject(detail.getPowerRecord(), ProPowerRecordDTO.class));
                }
                if (StringUtils.isNotBlank(detail.getStirFurnaceRecord())) {
                    result.setStirFurnaceRecordList(JSONArray.parseArray(detail.getStirFurnaceRecord(), ProStirFurnaceRecordDTO.class));
                }
                if (StringUtils.isNotBlank(detail.getOutFurnaceRecord())) {
                    result.setOutFurnaceRecordDTO(JSONObject.parseObject(detail.getOutFurnaceRecord(), ProOutFurnaceRecordDTO.class));
                }
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    /**
     * 根据炉号班次查询最新的记录
     *
     * @param furnaceId
     * @param shift
     * @param orgId
     * @param factoryId
     * @return
     */
    @Override
    public ProOreFurnaceRecord getLastestRecord(int furnaceId, int shift, int orgId, Integer factoryId) {
        // 添加查询分页减少网络传输数据
        Page page = new Page();
        page.setPageSize(3);
        Pagination pagination = PageUtils.transFromPage(page);
        List<ProOreFurnaceRecord> proOreFurnaceRecords = oreFurnaceRecordMapper.selectPage(pagination, new EntityWrapper<>(new ProOreFurnaceRecord()
                .setFurnaceId(furnaceId)
                .setShift(shift)
                .setStatus(Status.TRUE.getKey())
                .setFactoryId(factoryId)
                .setCompanyId(orgId)).orderBy("create_time", false));
        if (CollectionUtils.isEmpty(proOreFurnaceRecords)) {
            return null;
        }
        return proOreFurnaceRecords.get(0);
    }

    /**
     * @descriptin 条件查询矿热炉记录基本信息列表
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/11/5
     * @return
     */
    @Override
    public ResponseResult<List<ProOreFurnaceRecordDTO>> findOreRecords(ProOreFurnaceRecordRQ rq,
                                                                       AuthPlatformUserInfo userInfo,
                                                                       Pagination pagination) {
        EntityWrapper<ProOreFurnaceRecord> wrapper = new EntityWrapper<>(new ProOreFurnaceRecord()
                .setStatus(Status.TRUE.getKey())
                .setCompanyId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId()));
        if (!ObjectUtils.isEmpty(rq)) {
            if (!ObjectUtils.isEmpty(rq.getOnduty())) {
                wrapper.eq("onduty", rq.getOnduty());
                if (rq.getOnduty().equals(Status.TRUE.getKey())) {
                    wrapper.eq("create_id", userInfo.getId());
                }
            }
        }
        wrapper.orderBy("create_time", false);
        List<ProOreFurnaceRecord> records = baseMapper.selectPage(pagination, wrapper);
        List<ProOreFurnaceRecordDTO> result = BeanUtils.assemble(ProOreFurnaceRecordDTO.class, records);
        if (!ObjectUtils.isEmpty(records)) {
            List<Integer> userIds = new ArrayList<>();
            records.forEach(record -> {
                userIds.add(record.getCreateId());
            });
            Map<Integer, AuthPlatformUserInfo> users = userInfoUtils.getUserById(userInfo.getSysToken(), userIds);
            if (!ObjectUtils.isEmpty(users)) {
                result.forEach(record -> {
                    if (users.containsKey(record.getCreateId()))
                    record.setRecorder(users.get(record.getCreateId()).getName());
                });
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result, PageUtils.transToPage(pagination));
    }
}
