package com.bee.platform.cloud.si.manufacture.service.impl;


import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.RevisionStorageDetailMapper;
import com.bee.platform.cloud.si.manufacture.entity.RevisionStorageDetail;
import com.bee.platform.cloud.si.manufacture.service.RevisionStorageDetailService;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @ClassName: RevisionStorageDetailServiceImpl
 * @Description: 盘库记录Service
 * @Author: fei.sun
 * @Date: 2019/10/31 11:26
 * @Version: 1.0
 */
@Service
public class RevisionStorageDetailServiceImpl extends ServiceImpl<RevisionStorageDetailMapper,RevisionStorageDetail>
        implements RevisionStorageDetailService {

    @Override
    public boolean insertRevisionStorageDetails(List<RevisionStorageDetail> revisionStorageDetails) {
        return this.insertBatch(revisionStorageDetails);
    }
}
