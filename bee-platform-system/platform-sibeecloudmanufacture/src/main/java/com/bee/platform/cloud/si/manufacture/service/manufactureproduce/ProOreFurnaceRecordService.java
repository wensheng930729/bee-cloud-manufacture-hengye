package com.bee.platform.cloud.si.manufacture.service.manufactureproduce;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.ProOreFurnaceRecordDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProOreFurnaceRecordDetailDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProOreRecordUserDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProOreFurnaceRecord;
import com.bee.platform.cloud.si.manufacture.rq.ProOreFurnaceRecordDetailRQ;
import com.bee.platform.cloud.si.manufacture.rq.ProOreFurnaceRecordQueryRQ;
import com.bee.platform.cloud.si.manufacture.rq.ProOreFurnaceRecordRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 矿热炉记录 服务类
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-26
 */
public interface ProOreFurnaceRecordService extends IService<ProOreFurnaceRecord> {

    ResponseResult addOreRecord(ProOreFurnaceRecordRQ rq, AuthPlatformUserInfo userInfo);

    ResponseResult updateOreRecord(ProOreFurnaceRecordRQ rq, AuthPlatformUserInfo userInfo);

    ResponseResult<ProOreRecordUserDTO> getOreRecordOnduty(ProOreFurnaceRecordQueryRQ rq, AuthPlatformUserInfo userInfo);

    ResponseResult addOreRecordDetail(ProOreFurnaceRecordDetailRQ rq, AuthPlatformUserInfo userInfo);

    ResponseResult updateOreRecordDetail(ProOreFurnaceRecordDetailRQ rq, AuthPlatformUserInfo userInfo);

    ResponseResult<List<ProOreFurnaceRecordDetailDTO>> findOreRecordDetailList(Long recordId, AuthPlatformUserInfo userInfo);

    ResponseResult<ProOreFurnaceRecordDetailDTO> getOreRecordDetail(ProOreFurnaceRecordQueryRQ rq, AuthPlatformUserInfo userInfo);

    ResponseResult<ProOreFurnaceRecordDetailDTO> getOreRecordHandOverDetail(Integer furnaceId, AuthPlatformUserInfo userInfo);

    ProOreFurnaceRecord getLastestRecord(int furnaceId, int shift, int orgId, Integer factoryId);

    ResponseResult<List<ProOreFurnaceRecordDTO>> findOreRecords(ProOreFurnaceRecordRQ rq, AuthPlatformUserInfo userInfo,
                                                                Pagination pagination);
}
