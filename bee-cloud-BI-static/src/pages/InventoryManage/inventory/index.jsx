import { message } from 'antd';
import React, { useState, useEffect } from 'react';
import { connect } from 'dva';
import moment from 'moment';
import Link from 'umi/link';
import router from 'umi/router';
import { getList } from '../services/inventoryService'

import { FormItemRangePicker, TableWrapper } from '@/components/FormWidget';

const MyFormSearch = props => {
  const {
    form: { getFieldDecorator },
  } = props;
  const formItemLayout = {};

  return (
    <>
      <FormItemRangePicker
        getFieldDecorator={getFieldDecorator}
        label="盘点日期"
        fieldId="dateRange"
        required={false}
        formItemLayout={formItemLayout}
        datePickerProps={{ format: 'YYYY-MM-DD' }}
      />
    </>
  );
};

const Index = props => {
  const { dispatch } = props;

  const [sortOrder, setSortOrder] = useState('descend');

  useEffect(() => {
    // 这里在mount时执行一次
    dispatch({
      type: 'InventoryModel/fetchInitOptionsEffect',
      payload: {},
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  }, []);

  const columns = [
    {
      title: '盘点单号',
      dataIndex: 'inventoryOrderId',
      align:'center'
    },
    {
      title: '盘点日期',
      dataIndex: 'createTime',
      align:'center',
      sortOrder,
      sorter: (a, b) => {
        const yearA = a.createTime.slice(0, 4);
        const monthA = a.createTime.slice(5, 7);
        const dayA = a.createTime.slice(8, 10);

        const newAStr = `${yearA}-${monthA}-${dayA}`;

        const yearB = b.createTime.slice(0, 4);
        const monthB = b.createTime.slice(5, 7);
        const dayB = b.createTime.slice(8, 10);

        const newBStr = `${yearB}-${monthB}-${dayB}`;

        const bool = moment(newAStr).isBefore(newBStr);

        return bool;
      },
      ellipsis: true,
    },
    {
      title: '操作',
      dataIndex: 'operation',
      align:'center',
      render: (text, record) => (
        <Link
          to={{
            pathname: '/inventoryManage/inventory/detail',
            query: { inventoryOrderId: record.inventoryOrderId },
          }}
        >
          查看详情
        </Link>
      ),
    },
  ];

  const handleTableChange = (pagination, filter, sorter) => {
    if (sortOrder !== sorter.order) {
      setSortOrder(sorter.order);
    }
  };

  const jumpToEditPage = () => {
    router.push({
      pathname: '/inventoryManage/inventory/Edit',
      query: {
        a: 'aaaaa',
      },
    });
  };

  const CusTomTableProps = {
    listService: getList,
    searchParams: {
      dateRange: {
        startTime: moment()
          .subtract(30, 'days')
          .format('YYYY-MM-DD'),
        endTime: moment().format('YYYY-MM-DD'),
      },
    },
    tableProps: { columns },
    handleTableChange,
    onAddClick: jumpToEditPage,
  };
  return (
    <TableWrapper {...CusTomTableProps}>
      <MyFormSearch />
    </TableWrapper>
  );
};

export default connect(state => {
  const { } = state.InventoryModel;

  return {};
})(Index);
