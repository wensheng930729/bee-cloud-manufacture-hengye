package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.MaterialBatchDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProMaterialBatch;
import com.bee.platform.cloud.si.manufacture.rq.MaterialQueryRQ;

import java.util.List;

/**
 * <p>
 * 料批基本信息主表 Mapper 接口
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-24
 */
public interface ProMaterialBatchMapper extends BaseMapper<ProMaterialBatch> {

    List<MaterialBatchDTO> findList(MaterialQueryRQ rq, Pagination pagination);

}
