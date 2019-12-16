package com.bee.platform.cloud.si.manufacture.service.manufactureproduce;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.MaterialBatchDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProMaterialBatch;
import com.bee.platform.cloud.si.manufacture.rq.MaterialBatchRQ;
import com.bee.platform.cloud.si.manufacture.rq.MaterialQueryRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 料批基本信息主表 服务类
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-24
 */
public interface ProMaterialBatchService extends IService<ProMaterialBatch> {

    ResponseResult addMaterialBatch(MaterialBatchRQ rq, AuthPlatformUserInfo userInfo);

    ResponseResult updateMaterialBatch(MaterialBatchRQ rq, AuthPlatformUserInfo userInfo);

    ResponseResult<MaterialBatchDTO> findInfo(Long batchId);

    ResponseResult<List<MaterialBatchDTO>> findList(AuthPlatformUserInfo userInfo,
                                                           MaterialQueryRQ rq,
                                                           Pagination pagination);

    ResponseResult<List<MaterialBatchDTO>> findComboBoxList(AuthPlatformUserInfo userInfo, Integer havePlc);

    ResponseResult<MaterialBatchDTO>  findInfoByPlcId(Integer plcId);
}
