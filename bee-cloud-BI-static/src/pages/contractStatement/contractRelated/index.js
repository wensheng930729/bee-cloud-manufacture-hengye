import React, { Component } from 'react';
import { Card, message } from 'antd';
import { connect } from 'dva';
import StandardTable from '@/components/FormWidget/StandardTable';
import { CustomModalHOC } from '@/components/FormWidget';
import styles from '../index.less';

@connect(({ contractRelated, loading }) => ({
  listSearchParams: contractRelated.listSearchParams,
  dataObj: contractRelated.dataObj,
  loading: loading.effects['contractRelated/getListEffect'],
}))
class Index extends Component {
  componentDidMount() {
    const { productName, custOrSupName } = this.props;
    this.getTableData({ productName, custOrSupName });
  }

  // 分页改变事件
  handleTableChange = payload => {
    const { productName, custOrSupName } = this.props;
    const params = {
      productName,
      custOrSupName,
      currentPage: payload.current,
      pageSize: payload.pageSize,
    };

    this.getTableData({ ...params });
  };

  // 获取表格数据
  getTableData = (obj = {}) => {
    // 表格之前的查询条件
    const { listSearchParams, dispatch } = this.props;
    // listSearchParams.orderStage = 'desc';
    const params = { ...listSearchParams, ...obj };
    dispatch({
      type: 'contractRelated/getListEffect',
      payload: params,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  render() {
    const { dataObj, onSelectRow } = this.props;
    const columns = [
      {
        title: '合同编号',
        dataIndex: 'contractNum',
        key: 'contractNum',
      },
      {
        title: '客户/供应商',
        dataIndex: 'custOrSupName',
        key: 'custOrSupName',
      },
      {
        title: '产品名称',
        dataIndex: 'productName',
        key: 'productName',
      },
      {
        title: '合同数量',
        dataIndex: 'quantity',
        key: 'quantity',
      },
      {
        title: '合同金额',
        dataIndex: 'amount',
        key: 'amount',
      },
      {
        title: '合同签订时间',
        dataIndex: 'signDate',
        key: 'signDate',
      },
    ];
    const propsrowSelection = {
      type: 'radio',
    };
    const tableProps = {
      loading: false,
      columns,
      data: dataObj,
      onChange: this.handleTableChange,
      bordered: false,
      scroll: {},
      hasRowSelection: true,
      rowSelection: propsrowSelection,
      onSelectRow,
    };

    return (
      <Card className={styles.cardContainer}>
        <StandardTable {...tableProps} />
      </Card>
    );
  }
}
export default CustomModalHOC(Index);
