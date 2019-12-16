package com.bee.platform.cloud.si.manufacture.service.manufactureproduce.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProDeviceInspectionMapper;
import com.bee.platform.cloud.si.manufacture.dto.DeviceInspectionDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProDeviceInspection;
import com.bee.platform.cloud.si.manufacture.rq.ProDeviceInspectionConfirmRQ;
import com.bee.platform.cloud.si.manufacture.rq.ProDeviceInspectionRQ;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProDeviceInspectionService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-25
 */
@Slf4j
@Service
public class ProDeviceInspectionServiceImpl extends ServiceImpl<ProDeviceInspectionMapper, ProDeviceInspection> implements ProDeviceInspectionService {


    /**
     * @descriptin 新增巡检设备信息
     * @author xin.huang
     * @param rq
     * @param userInfo
     * @date 2019/9/25
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult add(ProDeviceInspectionRQ rq, AuthPlatformUserInfo userInfo) {
        ProDeviceInspection proDeviceInspection = BeanUtils.copyProperties(rq, ProDeviceInspection.class);
        proDeviceInspection.setCreateId(userInfo.getId())
                .setCompanyId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setComplete(Status.FALSE.getKey())
                .setCreateTime(new Date());
        if (!this.insert(proDeviceInspection)) {
            log.error("添加设备巡检信息失败 类：{} 方法：{}", "ProDeviceInspectionServiceImpl", "add");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @descriptin 查询巡检设备记录列表
     * @author xin.huang
     * @param userInfo
     * @date 2019/9/25
     * @return
     */
    @Override
    public ResponseResult<DeviceInspectionDTO> findDeviceInspectionList(AuthPlatformUserInfo userInfo) {
        DeviceInspectionDTO deviceInspection = new DeviceInspectionDTO();
        //获取当前巡检时间段的设备巡检信息
        List<ProDeviceInspection> proDeviceInspections = this.selectList(new EntityWrapper<>(new ProDeviceInspection()
                .setCompanyId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setStatus(Status.TRUE.getKey())
                .setComplete(Status.FALSE.getKey()))
                .orderBy("create_time", true));
        if (!ObjectUtils.isEmpty(proDeviceInspections)) {
            deviceInspection.setStartTime( proDeviceInspections.get(0).getCreateTime());
            //所有巡检设备
            List<String> totalDevices = new ArrayList<>(proDeviceInspections.size());
            //检修设备
            List<String> repairDevices = new ArrayList<>(proDeviceInspections.size());
            //异常设备
            List<String> abnormalDevices = new ArrayList<>(proDeviceInspections.size());

            List<Long> deviceIds = new ArrayList<>(proDeviceInspections.size());
            proDeviceInspections.forEach(dp -> {
                totalDevices.add(dp.getName());
                if (dp.getState().equals(2)) {
                    repairDevices.add(dp.getName());
                } else if (dp.getState().equals(3)) {
                    abnormalDevices.add(dp.getName());
                }
                deviceIds.add(dp.getId());
            });
            deviceInspection.setTotalDevices(totalDevices)
                    .setRepairDevices(repairDevices)
                    .setAbnormalDevices(abnormalDevices);
            deviceInspection.setIds(deviceIds);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, deviceInspection);
    }

    /**
     * @descriptin 设备巡检结果确认
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @date 2019/10/14
     * @return
     */
    @Override
    public ResponseResult confirmDeviceInspection(AuthPlatformUserInfo userInfo, ProDeviceInspectionConfirmRQ rq) {
        //更新设备状态为巡检完成
        if (ObjectUtils.isEmpty(rq.getIds())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS.DEVICE_INSPECTION_NOT_EXISTED);
        }
        this.update(new ProDeviceInspection().setComplete(Status.TRUE.getKey()),
                new EntityWrapper<ProDeviceInspection>().in("id", rq.getIds()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}
